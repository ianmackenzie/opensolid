module BoundingBox3d (BoundingBox3d (..)) where

import Bounds
import CoordinateSystem (Units)
import OpenSolid
import Range (Range)

type role BoundingBox3d nominal

data BoundingBox3d (coordinateSystem :: CoordinateSystem)
  = BoundingBox3d
      (Range (Units coordinateSystem))
      (Range (Units coordinateSystem))
      (Range (Units coordinateSystem))

instance Bounds (BoundingBox3d (space @ units))
