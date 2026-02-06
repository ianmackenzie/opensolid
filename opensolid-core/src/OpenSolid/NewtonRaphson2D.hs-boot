module OpenSolid.NewtonRaphson2D (curve, surface) where

import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D)

curve :: (Number -> Vector2D units space) -> (Number -> Vector2D units space) -> Number -> Number
surface ::
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  UvPoint ->
  UvPoint
