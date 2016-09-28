{-# LANGUAGE ExistentialQuantification #-}

module Control.Arrow.Transformer where

import Control.Arrow;

class ArrowTransformer xT where {
  lift :: Arrow r => r a b -> xT r a b;
  tmap :: (Arrow r, Arrow s) => (∀ a b . r a b -> s a b) -> xT r a b -> xT s a b;
};
