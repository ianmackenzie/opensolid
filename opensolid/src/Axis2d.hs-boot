module Axis2d
  ( Axis2d (Axis2d)
  , originPoint
  , direction
  )
where

import Direction2d (Direction2d)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)

type role Axis2d nominal

data Axis2d (coordinateSystem :: CoordinateSystem) where
  Axis2d :: Point2d (space @ units) -> Direction2d space -> Axis2d (space @ units)

originPoint :: Axis2d (space @ units) -> Point2d (space @ units)
direction :: Axis2d (space @ units) -> Direction2d space
