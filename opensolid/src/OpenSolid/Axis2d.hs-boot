module OpenSolid.Axis2d
  ( Axis2d
  , through
  , originPoint
  , direction
  , normalDirection
  )
where

import {-# SOURCE #-} Direction2d (Direction2d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Point2d (Point2d)

type role Axis2d nominal

data Axis2d (coordinateSystem :: CoordinateSystem)

through :: Point2d (space @ units) -> Direction2d space -> Axis2d (space @ units)
originPoint :: Axis2d (space @ units) -> Point2d (space @ units)
direction :: Axis2d (space @ units) -> Direction2d space
normalDirection :: Axis2d (space @ units) -> Direction2d space
