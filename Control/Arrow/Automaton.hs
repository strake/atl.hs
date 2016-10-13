{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , UndecidableInstances
  #-}

module Control.Arrow.Automaton where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Hoist

import Util

data AutomatonT a b c = AutomatonT { runAutomatonT :: a b (c, AutomatonT a b c) }

evalAutomatonT :: Arrow a => AutomatonT a b c -> a b c
evalAutomatonT (AutomatonT a) = a >>^ fst

execAutomatonT :: Arrow a => AutomatonT a b c -> a b (AutomatonT a b c)
execAutomatonT (AutomatonT a) = a >>^ snd

type Automaton = AutomatonT (->)

runAutomaton :: Automaton a b -> a -> (b, Automaton a b)
runAutomaton = runAutomatonT

evalAutomaton :: Automaton a b -> a -> b
evalAutomaton = evalAutomatonT

execAutomaton :: Automaton a b -> a -> Automaton a b
execAutomaton = execAutomatonT

instance ArrowTrans AutomatonT where
    lift a = AutomatonT $ proc x -> do
        y <- a -< x
        returnA -< (y, lift a)

instance ArrowHoist AutomatonT where
    hoistA f (AutomatonT a) = AutomatonT $ f $ proc x -> do
        (y, n) <- a -< x
        returnA -< (y, hoistA f n)

instance Arrow a => Category (AutomatonT a) where
    id = AutomatonT (id &&& constA id)
    AutomatonT a . AutomatonT b = AutomatonT $ proc x -> do
        (y, n) <- b -< x
        (z, m) <- a -< y
        returnA -< (z, m . n)

instance Arrow a => Arrow (AutomatonT a) where
    arr f = AutomatonT (arr f &&& constA (arr f))
    first (AutomatonT a) = AutomatonT $ proc (x, y) -> do
        (x', n) <- a -< x
        returnA -< ((x', y), first n)

instance ArrowZero a => ArrowZero (AutomatonT a) where
    zeroArrow = lift zeroArrow

instance ArrowPlus a => ArrowPlus (AutomatonT a) where
    AutomatonT a <+> AutomatonT b = AutomatonT (a <+> b)

instance ArrowChoice a => ArrowChoice (AutomatonT a) where
    left (AutomatonT a) = AutomatonT $ proc ex -> do
        case ex of
            Right x -> returnA -< (Right x, left (AutomatonT a))
            Left  x -> do
                (y, n) <- a -< x
                returnA -< (Left y, left n)

instance ArrowApply a => ArrowApply (AutomatonT a) where
    app = AutomatonT $ proc (AutomatonT a, x) -> do
        (y, n) <- app -< (a, x)
        returnA -< (y, app)

accumulateN :: (Integral n, ArrowApply a) => n -> AutomatonT a b c -> AutomatonT a b [c]
accumulateN 0 _ = constA []
accumulateN i (AutomatonT a) = AutomatonT $ proc x -> do
    (y, n) <- a -< x
    (ys, m) <- app -< (runAutomatonT $ accumulateN (i - 1) n, x)
    returnA -< (y:ys, m)

(>~>) :: Arrow a => AutomatonT a b c -> AutomatonT a b c -> AutomatonT a b c
a >~> n = AutomatonT (evalAutomatonT a &&& constA n)
