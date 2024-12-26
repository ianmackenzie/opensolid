module OpenSolid.Line3d
  ( from
  , Line3d
  , startPoint
  , endPoint
  )
where

import Data.Coerce qualified
import OpenSolid.Prelude
import OpenSolid.Curve3d (Curve3d)
import OpenSolid.Curve3d qualified as Curve3d
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve3d qualified as Expression.Curve3d
import OpenSolid.Expression.VectorCurve3d qualified as Expression.VectorCurve3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Units qualified as Units

from ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Curve3d (space @ units)
from p1 p2 =
  Curve3d.Parametric $
    Expression.Curve3d.constant p1 + Expression.t * Expression.VectorCurve3d.constant (p2 - p1)

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
