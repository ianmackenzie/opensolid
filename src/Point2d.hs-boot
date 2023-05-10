module Point2d (Point2d) where

import OpenSolid
import {-# SOURCE #-} Vector2d (Vector2d)

type role Point2d phantom

data Point2d (coordinateSystem :: CoordinateSystem)

instance
  (units ~ units', space ~ space')
  => Subtraction
      (Point2d (space @ units))
      (Point2d (space' @ units'))
      (Vector2d (space @ units))
