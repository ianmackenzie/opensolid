module OpenSolid.BezierCurve2d
  ( fromControlPoints
  , hermite
  )
where

import OpenSolid.Curve1d qualified as Curve1d
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d

{-| Construct a Bezier curve from its start point (first control point), inner control points and
end point (last control point). For example,

> BezierCurve2d.fromControlPoints p1 [ p2, p3 ] p4

will return a cubic spline with the given four control points.
-}
fromControlPoints :: NonEmpty (Point2d (space @ units)) -> Curve2d (space @ units)
fromControlPoints controlPoints =
  Curve2d.xy
    (Curve1d.bezier (NonEmpty.map Point2d.xCoordinate controlPoints))
    (Curve1d.bezier (NonEmpty.map Point2d.yCoordinate controlPoints))

{-| Construct a Bezier curve with the given start point, start derivatives, end point and end
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
  let Point2d xStart yStart = startPoint
  let Point2d xEnd yEnd = endPoint
  let xStartDerivatives = List.map Vector2d.xComponent startDerivatives
  let yStartDerivatives = List.map Vector2d.yComponent startDerivatives
  let xEndDerivatives = List.map Vector2d.xComponent endDerivatives
  let yEndDerivatives = List.map Vector2d.yComponent endDerivatives
  let x = Curve1d.hermite (xStart, xStartDerivatives) (xEnd, xEndDerivatives)
  let y = Curve1d.hermite (yStart, yStartDerivatives) (yEnd, yEndDerivatives)
  Curve2d.xy x y
