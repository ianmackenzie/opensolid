module Vector3d
  ( Vector3d
  , xyz
  , xComponent
  , yComponent
  , zComponent
  , components
  , transformBy
  )
where

import OpenSolid
import {-# SOURCE #-} Transform3d (Transform3d)
import Units qualified

type role Vector3d phantom

data Vector3d (coordinateSystem :: CoordinateSystem)

instance Eq (Vector3d (space @ units))

instance Show (Vector3d (space @ units))

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition (Vector3d (space @ units)) (Vector3d (space_ @ units_)) (Vector3d (space @ units))

instance Multiplication' (Qty units1) (Vector3d (space @ units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Vector3d (space @ units2)) (Vector3d (space @ units3))

instance Multiplication' (Vector3d (space @ units1)) (Qty units2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Qty units2) (Vector3d (space @ units3))

xyz :: Qty units -> Qty units -> Qty units -> Vector3d (space @ units)
xComponent :: Vector3d (space @ units) -> Qty units
yComponent :: Vector3d (space @ units) -> Qty units
zComponent :: Vector3d (space @ units) -> Qty units
components :: Vector3d (space @ units) -> (Qty units, Qty units, Qty units)
transformBy ::
  Transform3d tag (space @ units1) ->
  Vector3d (space @ units2) ->
  Vector3d (space @ units2)
