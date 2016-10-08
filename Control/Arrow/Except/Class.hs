{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Control.Arrow.Except.Class (
  -- * The ArrowError class
    ArrowError
  , throwError
  , catchError
) where

import Control.Arrow

-- | A class that represents an arrow which can throw and handle errors.
class Arrow a => ArrowError e a | a -> e where
    -- | @throwError@ is an arrow that takes the error to be thrown.
    throwError :: a e b

    {- | @catchError@ takes an arrow and an error handler and returns another
    arrows which, when it throws an error, this error is fed to the handler.
    -}
    catchError :: a b c  -- ^ The main computation
               -> a e c  -- ^ The error handler, executed if the computation threw an error
               -> a b c
