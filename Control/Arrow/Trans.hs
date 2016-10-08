{-# LANGUAGE Rank2Types #-}

module Control.Arrow.Trans (
    ArrowTrans
  , lift
) where

import Control.Arrow

class ArrowTrans t where
  lift :: Arrow a => a b c -> t a b c
