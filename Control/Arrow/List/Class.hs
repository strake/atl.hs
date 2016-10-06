module Control.Arrow.List.Class where

import Control.Arrow
import Control.Applicative

class Arrow a => ArrowList a where
    arrL :: (b  -> [c]) -> a b c
    mapL :: ([c] -> [d]) -> a b c -> a b d

instance ArrowList (Kleisli []) where
    arrL = Kleisli . arr
    mapL f (Kleisli k) = Kleisli (\ x -> f (k x))
