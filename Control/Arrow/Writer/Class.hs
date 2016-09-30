{-# LANGUAGE FunctionalDependencies #-}

module Control.Arrow.Writer.Class where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category

class Arrow a => ArrowWriter w a | a -> w where
    tell :: a w ()
    listen :: a b (b, w)
    censor :: (w -> w) -> a b c -> a b c

listenA :: ArrowWriter w a => a w c -> a b (b, c)
listenA x = listen >>> id *** x

listens :: ArrowWriter w a => (w -> c) -> a b (b, c)
listens = listenA . arr
