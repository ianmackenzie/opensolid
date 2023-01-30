module Axis2d
  ( Axis2d
  , through
  , direction
  , originPoint
  )
where

import Direction2d (Direction2d)
import Point2d (Point2d)

data Axis2d coordinates = Axis2d
  { originPoint :: Point2d coordinates
  , direction :: Direction2d coordinates
  }

through :: Point2d coordinates -> Direction2d coordinates -> Axis2d coordinates
through = Axis2d
