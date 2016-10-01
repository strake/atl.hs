{-# LANGUAGE
    Arrows
  , FunctionalDependencies
  #-}

module Control.Arrow.Reader.Class where

import Control.Arrow

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
