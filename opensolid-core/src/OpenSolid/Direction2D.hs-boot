module OpenSolid.Direction2D
  ( Direction2D
  , unsafe
  , unwrap
  , rotateLeft
  , parallel
  , perpendicular
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Direction2D, Vector2D)

unsafe :: Vector2D Unitless -> Direction2D
unwrap :: Direction2D -> Vector2D Unitless
rotateLeft :: Direction2D -> Direction2D
parallel :: Direction2D -> Direction2D -> Bool
perpendicular :: Direction2D -> Direction2D -> Bool
