{-# LANGUAGE FunctionalDependencies #-}

module Control.Arrow.Abort.Class where

import Control.Arrow;
import Control.Arrow.Transformer;

class Arrow r => ArrowAbort v r | r -> v where {
  abort :: r v a; -- terminate with final value
};
