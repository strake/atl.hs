{-# LANGUAGE FunctionalDependencies #-}

module Control.Arrow.State.Class where

import Control.Arrow

class Arrow a => ArrowState s a | a -> s where
    get :: a () s
    put :: a s ()

gets :: ArrowState s a => (s -> b) -> a () b
gets f = get >>> arr f

set :: ArrowState s a => (s -> s) -> a b b
set f = arr id &&& (arr (const ()) >>> get >>> arr f >>> put) >>> arr fst
