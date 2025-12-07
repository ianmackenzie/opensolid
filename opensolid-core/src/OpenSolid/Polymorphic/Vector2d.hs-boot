module OpenSolid.Polymorphic.Vector2d
  ( zero
  , coerce
  , normalize
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Frame2d, Transform2d, Vector2d)

zero :: Vector2d units space
coerce :: Vector2d units1 space1 -> Vector2d units2 space2
normalize :: Vector2d units space -> Vector2d Unitless space
transformBy :: Transform2d tag units1 space -> Vector2d units2 space -> Vector2d units2 space
placeIn :: Frame2d frameUnits global local -> Vector2d units local -> Vector2d units global
relativeTo :: Frame2d frameUnits global local -> Vector2d units global -> Vector2d units local
