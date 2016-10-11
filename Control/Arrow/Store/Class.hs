{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , UndecidableInstances
  #-}

module Control.Arrow.Store.Class (
  -- * The ArrowStore class
    ArrowStore
  , pos
  , peek, peeks
  , seek, seeks
) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow

import Util

-- | A context-dependent computation.
class Arrow a => ArrowStore s a | a -> s where
    -- | Returns the current context
    pos :: a () s

    -- | Executes a computation using a local context
    peek :: a () b -> a s b

    -- | Executes a computation using a local context derived from the current one.
    peeks :: a () b -> a (s -> s) b
    peeks a = proc f -> do
        s <- pos -< ()
        peek a -< f s

    -- | Sets the current context.
    seek :: a (s, b) c -> a b c
    seek a = proc x -> do
        s <- pos -< ()
        a -< (s, x)

    -- | Sets the current context depending on the previous one.
    seeks :: a (s, b) c -> a (s -> s, b) c
    seeks a = proc (f, x) -> do
        s <- pos -< ()
        a -< (f s, x)
