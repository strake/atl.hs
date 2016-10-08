{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Control.Arrow.RWS.Class (
  -- * The ArrowRWS class
    ArrowRWS
  -- * Re-exports
  , module Control.Arrow.Reader.Class
  , module Control.Arrow.Writer.Class
  , module Control.Arrow.State.Class
) where

import Control.Arrow.Reader.Class
import Control.Arrow.Writer.Class
import Control.Arrow.State.Class

import Data.Monoid

-- | Arrowized RWS monad
class (Monoid w, ArrowWriter r a, ArrowWriter w a, ArrowState s a) => ArrowRWS r w s a | a -> r w s
