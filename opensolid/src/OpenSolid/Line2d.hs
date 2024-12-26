module OpenSolid.Line2d
  ( from
  , Line2d
  , startPoint
  , endPoint
  )
where

import Data.Coerce qualified
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve2d qualified as Expression.Curve2d
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

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
