{-# LANGUAGE
    Arrows
  , FunctionalDependencies
  #-}

module Control.Arrow.Writer.Class where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category
import Control.Arrow.Kleisli
import qualified Control.Monad.Writer as M

import Data.Monoid

class Arrow a => ArrowWriter w a | a -> w where
    writer :: (b -> (c, w)) -> a b c
    writer f = proc x -> do
        (y, w) <- returnA -< f x
        tell -< w
        returnA -< y

    tell :: a w ()
    tell = writer (\ w -> ((), w))

    listen :: a b c -> a b (c, w)
    -- listen :: Kleisli m b c -> Kleisli m b (c, w)
    -- listen :: (b -> m b) -> (b -> m (c, w))

    pass :: a b (c, w -> w) -> a b c


censor :: ArrowWriter w a => (w -> w) -> a b c -> a b c
censor f a = pass $ proc x -> do
    y <- a -< x
    returnA -< (y, f)

listenA :: ArrowWriter w a => a w c -> a b (b, c)
listenA a = proc x -> do
    (_, w) <- listen id -< ()
    c <- a -< w
    returnA -< (x, c)

listens :: ArrowWriter w a => (w -> c) -> a b (b, c)
listens = listenA . arr

instance (Monoid w, Monad m) => ArrowWriter w (Kleisli (M.WriterT w m)) where
    tell = arrK M.tell
    listen k = Kleisli (\ x -> M.listen (runKleisli k x))
    pass k = Kleisli (\ x -> M.pass (runKleisli k x))
