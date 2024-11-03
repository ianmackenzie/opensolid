module Line2d
  ( from
  , Line2d
  , startPoint
  , endPoint
  )
where

import Curve2d (Curve2d)
import Curve2d qualified
import Data.Coerce qualified
import Expression qualified
import Expression.Curve2d qualified
import Expression.VectorCurve2d qualified
import OpenSolid
import Point2d (Point2d)
import Units qualified

from ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Curve2d (space @ units)
from p1 p2 =
  Curve2d.Parametric $
    Expression.Curve2d.constant p1 + Expression.t * Expression.VectorCurve2d.constant (p2 - p1)

type role Line2d phantom

data Line2d (coordinateSystem :: CoordinateSystem) = Line2d
  { startPoint :: Point2d coordinateSystem
  , endPoint :: Point2d coordinateSystem
  }

deriving instance Show (Line2d (space @ units))

instance HasUnits (Line2d (space @ units)) where
  type UnitsOf (Line2d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Line2d (space1 @ unitsA)) (Line2d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce
