module OpenSolid.Vector2D
  ( zero
  , coerce
  , normalize
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Frame2d, Transform2d, Vector2D)

zero :: Vector2D units space
coerce :: Vector2D units1 space1 -> Vector2D units2 space2
normalize :: Vector2D units space -> Vector2D Unitless space
transformBy :: Transform2d tag units1 space -> Vector2D units2 space -> Vector2D units2 space
placeIn :: Frame2d frameUnits global local -> Vector2D units local -> Vector2D units global
relativeTo :: Frame2d frameUnits global local -> Vector2D units global -> Vector2D units local
