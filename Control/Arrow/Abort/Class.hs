{-# LANGUAGE FunctionalDependencies #-}

module Control.Arrow.Abort.Class where

import Control.Arrow
import Control.Arrow.Transformer

class Arrow a => ArrowAbort v a | a -> v where
    abort :: a v b -- terminate with final value
