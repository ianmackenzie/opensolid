module QuadraticSpline3d (fromControlPoints) where

import Curve3d (Curve3d)
import Curve3d qualified
import Expression qualified
import OpenSolid
import Point3d (Point3d)

fromControlPoints ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Curve3d (space @ units)
fromControlPoints p1 p2 p3 =
  Curve3d.Parametric (Expression.quadraticSpline p1 p2 p3)
