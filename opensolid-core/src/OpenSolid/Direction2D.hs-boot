module OpenSolid.Direction2D
  ( Direction2D
  , unsafe
  , unwrap
  , rotateLeft
  , areParallel
  , areIndependent
  , arePerpendicular
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Direction2D, Vector2D)

unsafe :: Vector2D Unitless -> Direction2D
unwrap :: Direction2D -> Vector2D Unitless
rotateLeft :: Direction2D -> Direction2D
areParallel :: Direction2D -> Direction2D -> Bool
areIndependent :: Direction2D -> Direction2D -> Bool
arePerpendicular :: Direction2D -> Direction2D -> Bool
