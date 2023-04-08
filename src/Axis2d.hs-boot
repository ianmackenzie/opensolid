module Axis2d
  ( Axis2d (originPoint, direction)
  )
where

import CoordinateSystem (Space)
import Direction2d (Direction2d)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)

type role Axis2d phantom

data Axis2d (coordinateSystem :: CoordinateSystem) = Axis2d
  { originPoint :: Point2d coordinateSystem
  , direction :: Direction2d (Space coordinateSystem)
  }
