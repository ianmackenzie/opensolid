module QuadraticSpline2d (fromControlPoints) where

import Curve2d (Curve2d)
import Curve2d qualified
import Expression qualified
import OpenSolid
import Point2d (Point2d)

fromControlPoints ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Curve2d (space @ units)
fromControlPoints p1 p2 p3 =
  Curve2d.Parametric (Expression.quadraticSpline p1 p2 p3)
