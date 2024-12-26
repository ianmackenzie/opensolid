module OpenSolid.Point3d
  ( Point3d
  , origin
  , xyz
  , coordinates
  , transformBy
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.Vector3d (Vector3d)

type role Point3d nominal

data Point3d (coordinateSystem :: CoordinateSystem)

instance Eq (Point3d (space @ units))

instance Show (Point3d (space @ units))

instance HasUnits (Point3d (space @ units))

instance
  space1 ~ space2 =>
  Units.Coercion (Point3d (space1 @ unitsA)) (Point3d (space2 @ unitsB))

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Point3d (space2 @ units2))
    (Vector3d (space1 @ units1))

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Point3d (space1 @ units1))

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Point3d (space1 @ units1))

origin :: Point3d (space @ units)
xyz :: Qty units -> Qty units -> Qty units -> Point3d (space @ units)
coordinates :: Point3d (space @ units) -> (Qty units, Qty units, Qty units)
transformBy :: Transform3d tag (space @ units) -> Point3d (space @ units) -> Point3d (space @ units)
