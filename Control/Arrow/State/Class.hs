{-# LANGUAGE FunctionalDependencies #-}

module Control.Arrow.State.Class where

import Control.Arrow;

class Arrow r => ArrowState s r | r -> s where {
  get :: r ()  s;
  put :: r s  ();
};

gets :: ArrowState s r => (s -> a) -> r () a;
gets f = get >>> arr f;

set :: ArrowState s r => (s -> s) -> r a a;
set f = arr id &&& (arr (const ()) >>> get >>> arr f >>> put) >>> arr fst;
