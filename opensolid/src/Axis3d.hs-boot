module Axis3d
  ( Axis3d
  , through
  , originPoint
  , direction
  )
where

import {-# SOURCE #-} Direction3d (Direction3d)
import OpenSolid
import {-# SOURCE #-} Point3d (Point3d)

type role Axis3d nominal

data Axis3d (coordinateSystem :: CoordinateSystem)

through :: Point3d (space @ units) -> Direction3d space -> Axis3d (space @ units)
originPoint :: Axis3d (space @ units) -> Point3d (space @ units)
direction :: Axis3d (space @ units) -> Direction3d space
