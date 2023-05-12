module BoundingBox2d
  ( BoundingBox2d (..)
  , constant
  )
where

import Bounds (IsBounds)
import CoordinateSystem (Units)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import Range (Range)

type role BoundingBox2d phantom

data BoundingBox2d (coordinateSystem :: CoordinateSystem)
  = BoundingBox2d
      (Range (Units coordinateSystem))
      (Range (Units coordinateSystem))

instance IsBounds (BoundingBox2d (space @ units))

constant :: Point2d (space @ units) -> BoundingBox2d (space @ units)
