{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Control.Arrow.State.Class where

import Control.Arrow

class Arrow a => ArrowState s a | a -> s where
    get :: a () s
    put :: a s ()

gets :: ArrowState s a => a (s -> b) b
gets = proc f -> do
    s <- get -< ()
    returnA -< f s

modify :: ArrowState s a => a (s -> s) ()
modify = proc f -> do
    s <- get -< ()
    put -< f s

set :: ArrowState s a => (s -> s) -> a b b
set f = arr id &&& (arr (const ()) >>> get >>> arr f >>> put) >>> arr fst
