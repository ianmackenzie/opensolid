module BoundingBox2d
  ( BoundingBox2d (BoundingBox2d)
  , constant
  )
where

import Bounds (IsBounds)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import Range (Range)

type role BoundingBox2d nominal

data BoundingBox2d (coordinateSystem :: CoordinateSystem) where
  BoundingBox2d :: Range units -> Range units -> BoundingBox2d (space @ units)

instance IsBounds (BoundingBox2d (space @ units))

constant :: Point2d (space @ units) -> BoundingBox2d (space @ units)
