{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Control.Arrow.State (
    module Control.Arrow.State.Class
  , StateT(..)
) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.State.Class
import Control.Category
import Util

newtype StateT s a b c = StateT { runStateT :: a (b, s) (c, s) }

evalStateT :: Arrow a => StateT s a b c -> a (b, s) c
evalStateT a = runStateT a >>^ fst

execStateT :: Arrow a => StateT s a b c -> a (b, s) s
execStateT a = runStateT a >>^ snd

type State s = StateT s (->)

runState :: State s a b -> a -> s -> (b, s)
runState = curry . runStateT

evalState :: State s a b -> a -> s -> b
evalState = curry . evalStateT

execState :: State s a b -> a -> s -> s
execState = curry . execStateT

instance ArrowTrans (StateT s) where
    lift = StateT . (*** id)
    tmap f = StateT . f . runStateT

instance Category a => Category (StateT s a) where
    id = StateT id
    StateT x . StateT y = StateT (x . y)

instance Arrow a => Arrow (StateT s a) where
    arr = StateT . arr . (*** id)
    first = StateT . (>>>) swap_snds_A . (<<<) swap_snds_A . (*** id) . runStateT

instance ArrowApply a => ArrowApply (StateT s a) where
    app = StateT $ arr (\ ((StateT f, x), s) -> (f, (x, s))) >>> app

instance ArrowZero a => ArrowZero (StateT s a) where
    zeroArrow = StateT zeroArrow

instance ArrowPlus a => ArrowPlus (StateT s a) where
    StateT f <+> StateT g = StateT (f <+> g)

instance ArrowChoice a => ArrowChoice (StateT s a) where
    left (StateT x) = StateT (arr f >>> left x >>> arr g)
        where f (Left  x, s)   = Left  (x, s)
              f (Right y, s)   = Right (y, s)
              g (Left  (x, s)) = (Left  x, s)
              g (Right (y, s)) = (Right y, s)

instance ArrowLoop a => ArrowLoop (StateT s a) where
    loop = StateT . loop . (>>> swap_snds_A) . (<<< swap_snds_A) . runStateT

instance Arrow a => ArrowState s (StateT s a) where
    get = StateT $ arr $ \ (_, s) -> (s,  s)
    put = StateT $ arr $ \ (s, _) -> ((), s)

instance (Arrow a, ArrowTrans t, Arrow (t (StateT s a))) => ArrowState s (t (StateT s a)) where
    get = lift get
    put = lift put
