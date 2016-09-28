{-# LANGUAGE KindSignatures, ScopedTypeVariables #-}

module Control.Arrow.Writer (module Control.Arrow.Writer.Class, WriterT (..)) where

import Prelude hiding ((.), id);

import Control.Arrow;
import Control.Arrow.Transformer;
import Control.Arrow.Writer.Class;
import Control.Category;
import Data.Monoid;
import Util;

newtype WriterT w r a b = WriterT { runWriterT :: r a (b, w) };

instance Monoid w => ArrowTransformer (WriterT w) where {
  lift = WriterT . (&&& arr (const mempty));
  tmap f = WriterT . f . runWriterT;
};

instance (Monoid w, Arrow r) => Category (WriterT w r) where {
  id = lift id;
  WriterT x . WriterT y = WriterT $
                          arr (\ ((v, w2), w1) -> (v, w1 `mappend` w2)) <<< x *** id <<< y;
};

instance (Monoid w, Arrow r) => Arrow (WriterT w r) where {
  arr = WriterT . arr . (&&& const mempty);
  first = WriterT . (.) swap_snds_A . first . runWriterT;
};

instance (Monoid w, ArrowApply r) => ArrowApply (WriterT w r) where {
  app = WriterT $ arr runWriterT *** id >>> app;
};

instance (Monoid w, ArrowZero r) => ArrowZero (WriterT w r) where {
  zeroArrow = WriterT zeroArrow;
};

instance (Monoid w, ArrowPlus r) => ArrowPlus (WriterT w r) where {
  WriterT f <+> WriterT g = WriterT (f <+> g);
};

instance (Monoid w, ArrowChoice r) => ArrowChoice (WriterT w r) where {
  left (WriterT x) = WriterT $ let {
                       f (Left (x, w)) = (Left x, w);
                       f (Right y)     = (Right y, mempty);
                     } in left x >>> arr f;
};

instance (Monoid w, ArrowLoop r) => ArrowLoop (WriterT w r) where {
  loop = WriterT . loop . (>>> swap_snds_A) . runWriterT;
};

instance (Monoid w, Arrow r) => ArrowWriter w (WriterT w r) where {
  tell = WriterT $ constA () &&& id;
  look = lift (runWriterT id);
  censor = withWriterT;
};

withWriterTA :: (Arrow r) => r x w -> WriterT x r a b -> WriterT w r a b;
withWriterTA a = WriterT . (>>> id *** a) . runWriterT;

withWriterT :: (Arrow r) => (x -> w) -> WriterT x r a b -> WriterT w r a b;
withWriterT = withWriterTA . arr;
