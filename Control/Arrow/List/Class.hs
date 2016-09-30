module Control.Arrow.List.Class where

import Control.Arrow
import Control.Arrow.Transformer

class Arrow a => ArrowList a where
    arrL :: (b  -> [c]) -> a b c
    mapL :: ([c] -> [d]) -> a b c -> a b d
