{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Control.Arrow.Reader (
    module Control.Arrow.Reader.Class
  , ReaderT(..)
  , withReaderT
  , runReader
) where

import Prelude hiding ((.), id)

import Control.Monad
import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Reader.Class
import Util

newtype ReaderT r a b c = ReaderT { runReaderT :: a (b, r) c }

type Reader r = ReaderT r (->)

runReader :: Reader r a b -> a -> r -> b
runReader = curry . runReaderT

instance ArrowTrans (ReaderT r) where
    lift = ReaderT . (<<< arr fst)

instance Arrow a => Category (ReaderT r a) where
    id = ReaderT (arr fst)
    ReaderT f . ReaderT g = ReaderT (f <<< g *** id <<< id &&& arr snd)

instance Arrow a => Arrow (ReaderT r a) where
    arr = lift . arr
    first = ReaderT . (<<< swap_snds_A) . (*** id) . runReaderT

instance ArrowApply a => ArrowApply (ReaderT r a) where
    app = ReaderT (arr (\ ((ReaderT f, x), r) -> (f, (x, r))) >>> app)

instance ArrowZero a => ArrowZero (ReaderT r a) where
    zeroArrow = ReaderT zeroArrow

instance ArrowPlus a => ArrowPlus (ReaderT r a) where
    ReaderT f <+> ReaderT g = ReaderT (f <+> g)

instance ArrowChoice a => ArrowChoice (ReaderT r a) where
    left (ReaderT x) = ReaderT (arr f >>> left x)
        where f (Left  x, r) = Left (x, r)
              f (Right y, r) = Right y

instance ArrowLoop a => ArrowLoop (ReaderT r a) where
    loop = ReaderT . loop . (<<< swap_snds_A) . runReaderT

instance Arrow a => ArrowReader r (ReaderT r a) where
    ask = ReaderT (arr snd)
    local = withReaderT . arr

withReaderT :: Arrow a => a q r -> ReaderT r a b c -> ReaderT q a b c
withReaderT a = ReaderT . (<<< id *** a) . runReaderT
