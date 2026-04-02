{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson2D (curve, surface) where

import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D)

curve :: (Number -> (# Vector2D units, Vector2D units #)) -> Number -> Number
surface ::
  (UvPoint -> (# Vector2D units, Vector2D units, Vector2D units #)) ->
  UvPoint ->
  UvPoint
