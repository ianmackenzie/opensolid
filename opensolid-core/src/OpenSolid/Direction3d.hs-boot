module OpenSolid.Direction3d
  ( Direction3d
  , unsafe
  , unwrap
  , components
  , transformBy
  )
where

import OpenSolid.Prelude
import OpenSolid.Transform qualified as Transform
import {-# SOURCE #-} OpenSolid.Transform3d (Transform3d)
import {-# SOURCE #-} OpenSolid.Vector3d (Vector3d)

type role Direction3d phantom

newtype Direction3d (space :: Type) = Direction3d (Vector3d (space @ Unitless))

unsafe :: Vector3d (space @ Unitless) -> Direction3d space
unwrap :: Direction3d space -> Vector3d (space @ Unitless)
components :: Direction3d space -> (Float, Float, Float)
transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag (space @ units1) ->
  Direction3d space ->
  Direction3d space

instance Multiplication' (Qty units) (Direction3d space) (Vector3d (space @ (units :*: Unitless)))

instance Multiplication (Qty units) (Direction3d space) (Vector3d (space @ units))

instance Multiplication' (Direction3d space) (Qty units) (Vector3d (space @ (Unitless :*: units)))

instance Multiplication (Direction3d space) (Qty units) (Vector3d (space @ units))
