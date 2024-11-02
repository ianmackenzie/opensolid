module Line3d
  ( from
  , Line3d
  , startPoint
  , endPoint
  )
where

import Curve3d (Curve3d)
import Curve3d qualified
import Data.Coerce qualified
import Expression qualified
import OpenSolid
import Point3d (Point3d)
import Units qualified

from ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Curve3d (space @ units)
from p1 p2 = Curve3d.Parametric (p1 + Expression.t * (p2 - p1))

type role Line3d phantom

data Line3d (coordinateSystem :: CoordinateSystem) = Line3d
  { startPoint :: Point3d coordinateSystem
  , endPoint :: Point3d coordinateSystem
  }

deriving instance Show (Line3d (space @ units))

instance HasUnits (Line3d (space @ units)) where
  type UnitsOf (Line3d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Line3d (space1 @ unitsA)) (Line3d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce
