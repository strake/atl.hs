{-# LANGUAGE FunctionalDependencies #-}

module Control.Arrow.Reader.Class where

import Control.Arrow;
import Control.Arrow.Transformer;

class Arrow s => ArrowReader r s | s -> r where {
  ask :: s () r;
  local :: (r -> r) -> s a b -> s a b;
};

asks :: (ArrowReader r s) => (r -> a) -> s () a;
asks = asksA . arr;

asksA :: (ArrowReader r s) => s r a -> s () a;
asksA x = ask >>> x;
