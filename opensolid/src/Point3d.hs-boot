module Point3d
  ( Point3d
  , origin
  , xyz
  , coordinates
  , transformBy
  )
where

import OpenSolid
import {-# SOURCE #-} Transform3d (Transform3d)
import Units qualified
import {-# SOURCE #-} Vector3d (Vector3d)

type role Point3d phantom

data Point3d (coordinateSystem :: CoordinateSystem)

instance Eq (Point3d (space @ units))

instance Show (Point3d (space @ units))

instance HasUnits (Point3d (space @ units))

instance
  space1 ~ space2 =>
  Units.Coercion (Point3d (space1 @ unitsA)) (Point3d (space2 @ unitsB))

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Point3d (space @ units))
    (Point3d (space_ @ units_))
    (Vector3d (space @ units))

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Point3d (space @ units))
    (Vector3d (space_ @ units_))
    (Point3d (space @ units))

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Point3d (space @ units))
    (Vector3d (space_ @ units_))
    (Point3d (space @ units))

origin :: Point3d (space @ units)
xyz :: Qty units -> Qty units -> Qty units -> Point3d (space @ units)
coordinates :: Point3d (space @ units) -> (Qty units, Qty units, Qty units)
transformBy :: Transform3d tag (space @ units) -> Point3d (space @ units) -> Point3d (space @ units)
