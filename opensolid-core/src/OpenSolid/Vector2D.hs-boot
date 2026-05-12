module OpenSolid.Vector2D
  ( Vector2D
  , zero
  , coerce
  , magnitude
  , normalize
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Frame2D, Vector2D, VectorTransform2D)

zero :: Vector2D units
coerce :: Vector2D units1 -> Vector2D units2
magnitude :: Vector2D units -> Quantity units
normalize :: Tolerance units => Vector2D units -> Vector2D Unitless
transformBy :: VectorTransform2D tag -> Vector2D units -> Vector2D units
placeIn :: Frame2D frameUnits -> Vector2D units -> Vector2D units
relativeTo :: Frame2D frameUnits -> Vector2D units -> Vector2D units
