{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Control.Arrow.RWS (
  -- * The RWST arrow transformer
    RWST(..)
  , evalRWST
  , execRWST

  -- * The pure RWS arrow
  , RWS
  , runRWS
  , evalRWS
  , execRWS

  -- * Re-exports
  , module Control.Arrow.RWS.Class
) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Hoist
import Control.Category
import Util

import Control.Arrow.Reader.Class
import Control.Arrow.Writer.Class
import Control.Arrow.State.Class
import Control.Arrow.RWS.Class

import Data.Monoid

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

-- | A configuration-output-state computation
newtype RWST r w s a b c = RWST { runRWST :: a (b, r, s) (c, w, s) }

-- | Returns the state and the result and the output of a computation.
evalRWST :: Arrow a => RWST r w s a b c -> a (b, r, s) (c, w)
evalRWST a = runRWST a >>^ fst3 &&& snd3

-- | Returns the state and the output of a computation.
execRWST :: Arrow a => RWST r w s a b c -> a (b, r, s) (s, w)
execRWST a = runRWST a >>^ trd3 &&& snd3

-- | The pure RWS arrow.
type RWS r w s = RWST r w s (->)

-- | Returns the result, the output and the final state of the computation.
runRWS :: RWS r w s a b -> a -> r -> s -> (b, w, s)
runRWS = curry3 . runRWST

-- | Returns the state and the result and the output of a computation.
evalRWS :: RWS r w s a b -> a -> r -> s -> (b, w)
evalRWS = curry3 . evalRWST

-- | Returns the state and the output of a computation.
execRWS :: RWS r w s a b -> a -> r -> s -> (s, w)
execRWS = curry3 . execRWST

instance Monoid w => ArrowTrans (RWST r w s) where
    lift a = RWST $ proc (x, _, s) -> do
        y <- a -< x
        returnA -< (y, mempty, s)

instance Monoid w => ArrowHoist (RWST r w s) where
    hoistA f (RWST a) = RWST (f a)

instance (Monoid w, Arrow a) => Category (RWST r w s a) where
    id = RWST $ arr (\ (x, _, s) -> (x, mempty, s))
    RWST a . RWST b = RWST $ proc (x, r, s) -> do
        (y, w, s')   <- b -< (x, r, s)
        (z, w', s'') <- a -< (y, r, s')
        returnA -< (z, w <> w', s'')

instance (Monoid w, Arrow a) => Arrow (RWST r w s a) where
    arr f = RWST $ arr (\ (x, _, s) -> (f x, mempty, s))
    first (RWST a) = RWST $ proc ((x, y), r, s) -> do
        (z, w, s') <- a -< (x, r, s)
        returnA -< ((z, y), w, s')

instance (Monoid w, ArrowZero a) => ArrowZero (RWST r w s a) where
    zeroArrow = lift zeroArrow

instance (Monoid w, ArrowPlus a) => ArrowPlus (RWST r w s a) where
    RWST a <+> RWST b = RWST (a <+> b)

instance (Monoid w, ArrowChoice a) => ArrowChoice (RWST r w s a) where
    left (RWST a) = RWST $ proc (ex, r, s) -> do
        case ex of
            Right x -> returnA -< (Right x, mempty, s)
            Left  x -> do
                (y, w, s') <- a -< (x, r, s)
                returnA -< (Left y, w, s')

instance (Monoid w, ArrowApply a) => ArrowApply (RWST r w s a) where
    app = RWST $ proc ((RWST a, x), r, s) -> app -< (a, (x, r, s))

-- todo: implement ArrowLoop

instance (Monoid w, Arrow a) => ArrowReader r (RWST r w s a) where
    ask = RWST $ arr (\ (_, r, s) -> (r, mempty, s))
    local f (RWST a) = RWST $ proc (x, r, s) -> do
        (y, w, s) <- a -< (x, f r, s)
        returnA -< (y, w, s)

instance (Monoid w, Arrow a) => ArrowWriter w (RWST r w s a) where
    tell = RWST $ arr (\ (w, _, s) -> ((), w, s))
    listen (RWST a) = RWST $ proc (x, r, s) -> do
        (y, w, s') <- a -< (x, r, s)
        returnA -< ((y, w), w, s')
    pass (RWST a) = RWST $ proc (x, r, s) -> do
        ((y, f), w, s') <- a -< (x, r, s)
        returnA -< (y, f w, s')

instance (Monoid w, Arrow a) => ArrowState s (RWST r w s a) where
    get = RWST $ arr (\ (_, r, s) -> (s, mempty, s))
    put = RWST $ arr (\ (s, r, _) -> ((), mempty, s))
