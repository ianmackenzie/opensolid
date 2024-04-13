module Direction2d
  ( Direction2d
  , unsafe
  , vector
  )
where

import OpenSolid
import {-# SOURCE #-} Vector2d (Vector2d)

type role Direction2d phantom

newtype Direction2d (space :: Type) = Direction2d (Vector2d (space @ Unitless))

unsafe :: Vector2d (space @ Unitless) -> Direction2d space
vector :: Direction2d space -> Vector2d (space @ Unitless)

instance Multiplication (Qty units) (Direction2d space)

instance Product (Qty units) (Direction2d space) (Vector2d (space @ units))

instance Multiplication (Direction2d space) (Qty units)

instance Product (Direction2d space) (Qty units) (Vector2d (space @ units))
