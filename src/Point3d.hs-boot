module Point3d (Point3d) where

import OpenSolid
import {-# SOURCE #-} Vector3d (Vector3d)

type role Point3d phantom

data Point3d (coordinateSystem :: CoordinateSystem)

instance
  (space ~ space', units ~ units') =>
  Subtraction
    (Point3d (space @ units))
    (Point3d (space' @ units'))
    (Vector3d (space @ units))
