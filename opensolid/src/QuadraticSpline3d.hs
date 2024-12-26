module QuadraticSpline3d (fromControlPoints) where

import OpenSolid.Prelude
import OpenSolid.Curve3d (Curve3d)
import OpenSolid.Curve3d qualified as Curve3d
import OpenSolid.Expression qualified as Expression
import OpenSolid.Point3d (Point3d)

fromControlPoints ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Curve3d (space @ units)
fromControlPoints p1 p2 p3 =
  Curve3d.Parametric (Expression.quadraticSpline p1 p2 p3)
