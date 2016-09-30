{-# LANGUAGE FunctionalDependencies #-}

module Control.Arrow.Reader.Class where

import Control.Arrow

class Arrow a => ArrowReader r a | a -> r where
    ask :: a () r
    local :: (r -> r) -> a b c -> a b c

asks :: ArrowReader r a => (r -> b) -> a () b
asks = asksA . arr

asksA :: ArrowReader r a => a r b -> a () b
asksA = (ask >>>)
