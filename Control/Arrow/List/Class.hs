module Control.Arrow.List.Class where

import Control.Arrow

class Arrow a => ArrowList a where
    arrL :: (b  -> [c]) -> a b c
    mapL :: ([c] -> [d]) -> a b c -> a b d
