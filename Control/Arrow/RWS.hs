{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Control.Arrow.RWS (
    module Control.Arrow.RWS.Class
  , RWST(..)
) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Arrow.Trans
import Control.Category
import Util

import Control.Arrow.Reader.Class
import Control.Arrow.Writer.Class
import Control.Arrow.State.Class
import Control.Arrow.RWS.Class

import Data.Monoid

newtype RWST r w s a b c = RWST { runRWST :: a (b, r, s) (c, w, s) }

instance Monoid w => ArrowTrans (RWST r w s) where
    lift a = RWST $ proc (x, _, s) -> do
        y <- a -< x
        returnA -< (y, mempty, s)

    tmap f = RWST . f . runRWST

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
