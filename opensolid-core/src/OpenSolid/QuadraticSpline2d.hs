module OpenSolid.QuadraticSpline2d (fromControlPoints) where

import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Expression qualified as Expression
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude

fromControlPoints ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Curve2d (space @ units)
fromControlPoints p1 p2 p3 =
  Curve2d.Parametric (Expression.quadraticSpline p1 p2 p3)
