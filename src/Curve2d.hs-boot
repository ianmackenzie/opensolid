module Curve2d
  ( Curve2d (Curve2d)
  , IsCurve2d
  )
where

import Angle (Angle)
import OpenSolid
import Point2d (Point2d)

class Show curve => IsCurve2d curve (coordinateSystem :: CoordinateSystem) | curve -> coordinateSystem where

type role Curve2d nominal

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Line :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
  Arc :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
  Curve2d :: IsCurve2d curve (space @ units) => curve -> Curve2d (space @ units)
