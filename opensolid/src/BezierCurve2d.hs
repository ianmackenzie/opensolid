module BezierCurve2d
  ( fromControlPoints
  , hermite
  )
where

import Curve2d (Curve2d)
import Curve2d qualified
import Expression qualified
import Float qualified
import Int qualified
import List qualified
import OpenSolid
import Point2d (Point2d)
import Vector2d (Vector2d)
import Vector2d qualified

{- | Construct a Bezier curve from its start point (first control point), inner control points and
end point (last control point). For example,

> BezierCurve2d.fromControlPoints p1 [ p2, p3 ] p4

will return a cubic spline with the given four control points.
-}
fromControlPoints :: NonEmpty (Point2d (space @ units)) -> Curve2d (space @ units)
fromControlPoints = Curve2d.Parametric . Expression.bezierCurve

{- | Construct a Bezier curve with the given start point, start derivatives, end point and end
derivatives. For example,

> BezierCurve2d.hermite (p1, [v1]) (p2, [v2])

will result in a cubic spline from @p1@ to @p2@ with first derivative equal to @v1@ at @p1@ and
first derivative equal to @v2@ at @p2@.

The numbers of derivatives at each endpoint do not have to be equal; for example,

> BezierCurve2d.hermite (p1, [v1]) (p2, [])

will result in a quadratic spline from @p1@ to @p2@ with first derivative at @p1@ equal to @v1@.

In general, the degree of the resulting spline will be equal to 1 plus the total number of
derivatives given.
-}
hermite ::
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  Curve2d (space @ units)
hermite (startPoint, startDerivatives) (endPoint, endDerivatives) = do
  let numStartDerivatives = List.length startDerivatives
  let numEndDerivatives = List.length endDerivatives
  let curveDegree = 1 + numStartDerivatives + numEndDerivatives
  let scaledStartDerivatives =
        scaleDerivatives Positive 1.0 (Float.int curveDegree) startDerivatives
  let scaledEndDerivatives =
        scaleDerivatives Negative 1.0 (Float.int curveDegree) endDerivatives
  let startControlPoints =
        derivedControlPoints startPoint 1 (numStartDerivatives + 1) scaledStartDerivatives
  let endControlPoints =
        List.reverse (derivedControlPoints endPoint 1 (numEndDerivatives + 1) scaledEndDerivatives)
  let controlPoints = startPoint :| (startControlPoints + endControlPoints + [endPoint])
  fromControlPoints controlPoints

scaleDerivatives ::
  Sign ->
  Float ->
  Float ->
  List (Vector2d (space @ units)) ->
  List (Vector2d (space @ units))
scaleDerivatives _ _ _ [] = []
scaleDerivatives sign scale n (first : rest) = do
  let updatedScale = sign * scale / n
  updatedScale * first : scaleDerivatives sign updatedScale (n - 1) rest

offset :: Int -> List (Vector2d (space @ units)) -> Vector2d (space @ units)
offset i scaledDerivatives =
  List.take i scaledDerivatives
    |> List.mapWithIndex (\j q -> Int.choose (i - 1) j * q)
    |> Vector2d.sum

derivedControlPoints ::
  Point2d (space @ units) ->
  Int ->
  Int ->
  List (Vector2d (space @ units)) ->
  List (Point2d (space @ units))
derivedControlPoints previousPoint i n qs
  | i < n = do
      let newPoint = previousPoint + offset i qs
      newPoint : derivedControlPoints newPoint (i + 1) n qs
  | otherwise = []
