module BoundingBox2d (BoundingBox2d (..)) where

import Bounds
import CoordinateSystem (Units)
import OpenSolid
import Range (Range)

type role BoundingBox2d phantom

data BoundingBox2d (coordinateSystem :: CoordinateSystem)
  = BoundingBox2d
      (Range (Units coordinateSystem))
      (Range (Units coordinateSystem))

instance Bounds (BoundingBox2d (space @ units))
