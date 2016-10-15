{-# LANGUAGE Rank2Types #-}

module Control.Arrow.Trans (
  --  * The ArrowTrans class
    ArrowTrans
  , lift
) where

import Control.Arrow

-- | A class allowing nested arrows.
class ArrowTrans t where
    -- | Lifts an arrow to the upper transformed level.
    lift :: Arrow a => a b c -> t a b c
