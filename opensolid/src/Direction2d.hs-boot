module Direction2d
  ( Direction2d
  , unsafe
  , unwrap
  , components
  , transformBy
  )
where

import OpenSolid
import Transform qualified
import {-# SOURCE #-} Transform2d (Transform2d)
import {-# SOURCE #-} Vector2d (Vector2d)

type role Direction2d phantom

newtype Direction2d (space :: Type) = Unit (Vector2d (space @ Unitless))

unsafe :: Vector2d (space @ Unitless) -> Direction2d space
unwrap :: Direction2d space -> Vector2d (space @ Unitless)
components :: Direction2d space -> (Float, Float)
transformBy ::
  Transform.IsOrthonormal tag =>
  Transform2d tag (space @ units1) ->
  Direction2d space ->
  Direction2d space

instance Multiplication' (Qty units) (Direction2d space)

instance Multiplication (Qty units) (Direction2d space) (Vector2d (space @ units))

instance Multiplication' (Direction2d space) (Qty units)

instance Multiplication (Direction2d space) (Qty units) (Vector2d (space @ units))
