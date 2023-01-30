module Axis2d
  ( Axis2d
  , direction
  , originPoint
  )
where

import Direction2d (Direction2d)
import {-# SOURCE #-} Point2d (Point2d)

type role Axis2d phantom

data Axis2d coordinates

originPoint :: Axis2d coordinates -> Point2d coordinates
direction :: Axis2d coordinates -> Direction2d coordinates
