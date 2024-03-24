module Direction2d
  ( Direction2d
  , unsafe
  , unwrap
  )
where

import OpenSolid
import {-# SOURCE #-} Vector2d (Vector2d)

type role Direction2d nominal

newtype Direction2d (space :: Type) = Direction2d_ (Vector2d (space @ Unitless))

unsafe :: Vector2d (space @ Unitless) -> Direction2d space
unwrap :: Direction2d space -> Vector2d (space @ Unitless)

instance Multiplication (Qty units) (Direction2d space)

instance Product (Qty units) (Direction2d space) (Vector2d (space @ units))

instance Multiplication (Direction2d space) (Qty units)

instance Product (Direction2d space) (Qty units) (Vector2d (space @ units))
