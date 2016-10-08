{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Control.Arrow.Reader.Class (
    ArrowReader
  , reader
  , ask
  , local
  , asks
) where

import Control.Arrow
import Control.Arrow.Kleisli

import qualified Control.Monad.Reader as M

class Arrow a => ArrowReader r a | a -> r where
    reader :: (b -> r -> c) -> a b c
    reader f = proc x -> do
        r <- ask -< ()
        returnA -< f x r

    ask :: a () r
    ask = reader (\ _ r -> r)

    local :: (r -> r) -> a b c -> a b c

asks :: ArrowReader r a => a (r -> b) b
asks = proc f -> do
    r <- ask -< ()
    returnA -< f r

instance Monad m => ArrowReader r (Kleisli (M.ReaderT r m)) where
    ask = liftK M.ask
    local f k = Kleisli (\ x -> M.local f (runKleisli k x))
