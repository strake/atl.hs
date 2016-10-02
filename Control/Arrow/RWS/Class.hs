{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Control.Arrow.RWS.Class where

import Control.Arrow.Reader.Class
import Control.Arrow.Writer.Class
import Control.Arrow.State.Class

import Data.Monoid

class (Monoid w, ArrowWriter r a, ArrowWriter w a, ArrowState s a) => ArrowRWS r w s a | a -> r w s
