{-# LANGUAGE
    Arrows
  , KindSignatures
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , FlexibleInstances
  #-}

module Control.Arrow.Writer (
    module Control.Arrow.Writer.Class
  , WriterT(..)
) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Writer.Class
import Control.Category
import Data.Monoid
import Util

newtype WriterT w a b c = WriterT { runWriterT :: a b (c, w) }

evalWriterT :: Arrow a => WriterT w a b c -> a b c
evalWriterT a = runWriterT a >>^ fst

execWriterT :: Arrow a => WriterT w a b c -> a b w
execWriterT a = runWriterT a >>^ snd

type Writer w = WriterT w (->)

runWriter :: Writer w a b -> a -> (b, w)
runWriter = runWriterT

evalWriter :: Writer w a b -> a -> b
evalWriter = evalWriterT

execWriter :: Writer w a b -> a -> w
execWriter = execWriterT

instance Monoid w => ArrowTrans (WriterT w) where
    lift = WriterT . (&&& arr (const mempty))
    tmap f = WriterT . f . runWriterT

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


withWriterTA :: Arrow a => a w w' -> WriterT w a b c -> WriterT w' a b c
withWriterTA a = WriterT . (>>> id *** a) . runWriterT

withWriterT :: Arrow a => (w -> w') -> WriterT w a b c -> WriterT w' a b c
withWriterT = withWriterTA . arr
