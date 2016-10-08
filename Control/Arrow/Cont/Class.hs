{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Control.Arrow.Cont.Class (
    ArrowCont
  , callCC
) where

import Control.Arrow

class Arrow a => ArrowCont r a | a -> r where
    callCC :: (a c r -> a b c) -> a b c
