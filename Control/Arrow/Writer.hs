{-# LANGUAGE
    Arrows
  , KindSignatures
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , FlexibleInstances
  #-}

module Control.Arrow.Writer (
  -- * The WriterT arrow transformer
    WriterT(..)
  , evalWriterT
  , execWriterT

  -- * The pure Writer arrow
  , Writer
  , runWriter
  , evalWriter
  , execWriter

  -- * Helper functions
  , withWriterT
  , withWriterTA

  -- * Re-exports
  , module Control.Arrow.Writer.Class
) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Hoist
import Control.Category
import Data.Monoid
import Util

import Control.Arrow.Writer.Class

-- | An arrow which outputs a result evaluated along the computation.
newtype WriterT w a b c = WriterT { runWriterT :: a b (c, w) }

-- | Returns only the result of the computation.
evalWriterT :: Arrow a => WriterT w a b c -> a b c
evalWriterT a = runWriterT a >>^ fst

-- | Returns only the output of the computation.
execWriterT :: Arrow a => WriterT w a b c -> a b w
execWriterT a = runWriterT a >>^ snd

type Writer w = WriterT w (->)

-- | Returns the result and the output of the computation.
runWriter :: Writer w a b -> a -> (b, w)
runWriter = runWriterT

-- | Returns only the result of the computation.
evalWriter :: Writer w a b -> a -> b
evalWriter = evalWriterT

-- | Returns only the output of the computation.
execWriter :: Writer w a b -> a -> w
execWriter = execWriterT

instance Monoid w => ArrowTrans (WriterT w) where
    lift = WriterT . (&&& arr (const mempty))

instance Monoid w => ArrowHoist (WriterT w) where
    hoistA f (WriterT a) = WriterT (f a)

instance (Monoid w, Arrow a) => Category (WriterT w a) where
    id = lift id
    WriterT x . WriterT y = WriterT (arr (\ ((v, w2), w1) -> (v, w1 `mappend` w2)) <<< x *** id <<< y)

instance (Monoid w, Arrow a) => Arrow (WriterT w a) where
    arr = WriterT . arr . (&&& const mempty)
    first = WriterT . (.) swap_snds_A . first . runWriterT

instance (Monoid w, ArrowApply a) => ArrowApply (WriterT w a) where
    app = WriterT $ arr runWriterT *** id >>> app

instance (Monoid w, ArrowZero a) => ArrowZero (WriterT w a) where
    zeroArrow = WriterT zeroArrow

instance (Monoid w, ArrowPlus a) => ArrowPlus (WriterT w a) where
    WriterT f <+> WriterT g = WriterT (f <+> g)

instance (Monoid w, ArrowChoice a) => ArrowChoice (WriterT w a) where
    left (WriterT x) = WriterT (left x >>> arr f)
        where  f (Left (x, w)) = (Left x, w)
               f (Right y)     = (Right y, mempty)

instance (Monoid w, ArrowLoop a) => ArrowLoop (WriterT w a) where
    loop = WriterT . loop . (>>> swap_snds_A) . runWriterT

instance (Monoid w, Arrow a) => ArrowWriter w (WriterT w a) where
    tell = WriterT $ constA () &&& id
    listen = lift . runWriterT
    pass a = WriterT $ proc x -> do
        ((y, f), w) <- runWriterT a -< x
        returnA -< (y, f w)

-- | Executes a computation in a temporarily modified state.
withWriterTA :: Arrow a => a w w' -> WriterT w a b c -> WriterT w' a b c
withWriterTA a = WriterT . (>>> id *** a) . runWriterT

-- | Executes a computation in a temporarily modified state.
withWriterT :: Arrow a => (w -> w') -> WriterT w a b c -> WriterT w' a b c
withWriterT = withWriterTA . arr
