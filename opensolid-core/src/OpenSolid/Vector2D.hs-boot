module OpenSolid.Vector2D
  ( Vector2D
  , zero
  , coerce
  , normalize
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Frame2D, Transform2D, Vector2D)

zero :: Vector2D units
coerce :: Vector2D units1 -> Vector2D units2
normalize :: Tolerance units => Vector2D units -> Vector2D Unitless
transformBy :: Transform2D tag units1 -> Vector2D units2 -> Vector2D units2
placeIn :: Frame2D frameUnits -> Vector2D units -> Vector2D units
relativeTo :: Frame2D frameUnits -> Vector2D units -> Vector2D units
