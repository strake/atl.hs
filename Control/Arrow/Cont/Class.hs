{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Control.Arrow.Cont.Class (
  -- * The ArrowCont class
    ArrowCont
  , callCC
) where

import Control.Arrow

-- | A CPS (Continuation Passing Style) arrow
class Arrow a => ArrowCont r a | a -> r where
    {- | The @callCC@ (call-with-current-continuation) function takes the
    current continuation as its argument and returns another continuation.
    -}
    callCC :: (a c r -> a b c)  -- ^ The current continuation
           -> a b c
