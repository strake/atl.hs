module Control.Arrow.List.Class where

import Control.Arrow;
import Control.Arrow.Transformer;

class Arrow r => ArrowList r where {
  arrL :: ( a  -> [b]) -> r a b;
  mapL :: ([b] -> [c]) -> r a b -> r a c;
};
