{-# LANGUAGE FunctionalDependencies #-}

module Control.Arrow.Except.Class where

import Control.Arrow

class Arrow a => ArrowError e a | a -> e where
    throwError :: a e b
    catchError :: a b c -> a e c -> a b c
