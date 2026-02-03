module OpenSolid.Direction2D
  ( Direction2D
  , unsafe
  , unwrap
  , rotateLeft
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Direction2D, Vector2D)

unsafe :: Vector2D Unitless space -> Direction2D space
unwrap :: Direction2D space -> Vector2D Unitless space
rotateLeft :: Direction2D space -> Direction2D space
