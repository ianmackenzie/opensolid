module BoundingBox3d
  ( BoundingBox3d (..)
  , constant
  )
where

import Bounds (IsBounds)
import CoordinateSystem (Units)
import OpenSolid
import {-# SOURCE #-} Point3d (Point3d)
import Range (Range)

type role BoundingBox3d phantom

data BoundingBox3d (coordinateSystem :: CoordinateSystem)
  = BoundingBox3d
      (Range (Units coordinateSystem))
      (Range (Units coordinateSystem))
      (Range (Units coordinateSystem))

instance IsBounds (BoundingBox3d (space @ units))

constant :: Point3d (space @ units) -> BoundingBox3d (space @ units)
