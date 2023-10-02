module BoundingBox3d
  ( BoundingBox3d (BoundingBox3d)
  , constant
  )
where

import Bounds (IsBounds)
import OpenSolid
import {-# SOURCE #-} Point3d (Point3d)
import Range (Range)

type role BoundingBox3d nominal

data BoundingBox3d (coordinateSystem :: CoordinateSystem) where
  BoundingBox3d :: Range units -> Range units -> Range units -> BoundingBox3d (space @ units)

instance IsBounds (BoundingBox3d (space @ units))

constant :: Point3d (space @ units) -> BoundingBox3d (space @ units)
