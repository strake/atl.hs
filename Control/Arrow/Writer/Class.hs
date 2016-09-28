{-# LANGUAGE FunctionalDependencies #-}

module Control.Arrow.Writer.Class where

import Prelude hiding ((.), id);

import Control.Arrow;
import Control.Category;

class Arrow r => ArrowWriter w r | r -> w where {
  tell :: r w ();
  look :: r a (a, w);
  censor :: (w -> w) -> r a b -> r a b;
};

looksA :: ArrowWriter w r => r w b -> r a (a, b);
looksA x = look >>> id *** x;

looks :: ArrowWriter w r => (w -> b) -> r a (a, b);
looks = looksA . arr;
