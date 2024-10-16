module BezierCurve2d
  ( fromControlPoints
  , hermite
  , expression
  )
where

import Bounds2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Expression (Expression)
import Expression qualified
import Expression.Curve2d qualified
import Float qualified
import Int qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Range (Range (Range))
import Transform2d (Transform2d)
import Vector2d (Vector2d)
import Vector2d qualified
import VectorCurve2d qualified

newtype BezierCurve2d (coordinateSystem :: CoordinateSystem)
  = BezierCurve2d (NonEmpty (Point2d coordinateSystem))

deriving instance Show (BezierCurve2d (space @ units))

deCasteljau :: Float -> NonEmpty (Point2d (space @ units)) -> Point2d (space @ units)
deCasteljau _ (point :| []) = point
deCasteljau t (p1 :| p2 : rest) = deCasteljau t (deCasteljauStep t p1 p2 rest)

deCasteljauStep ::
  Float ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  List (Point2d (space @ units)) ->
  NonEmpty (Point2d (space @ units))
deCasteljauStep t p1 p2 rest = do
  let point = Point2d.interpolateFrom p1 p2 t
  case rest of
    [] -> NonEmpty.singleton point
    p3 : remaining -> NonEmpty.prepend point (deCasteljauStep t p2 p3 remaining)

segmentControlPoints ::
  Float ->
  Float ->
  NonEmpty (Point2d (space @ units)) ->
  NonEmpty (Point2d (space @ units))
segmentControlPoints a b controlPoints =
  NonEmpty.map (segmentControlPoint a b controlPoints) $
    NonEmpty.range 0 (NonEmpty.length controlPoints - 1)

segmentControlPoint ::
  Float ->
  Float ->
  NonEmpty (Point2d (space @ units)) ->
  Int ->
  Point2d (space @ units)
segmentControlPoint _ _ (point :| []) _ = point
segmentControlPoint a b (p1 :| p2 : ps) n = do
  let t = if n > 0 then b else a
  let reduced = deCasteljauStep t p1 p2 ps
  segmentControlPoint a b reduced (n - 1)

controlPointDifferences ::
  Float ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  List (Point2d (space @ units)) ->
  NonEmpty (Vector2d (space @ units))
controlPointDifferences scale p1 p2 rest = do
  let v1 = scale * (p2 - p1)
  case rest of
    [] -> NonEmpty.singleton v1
    p3 : remaining -> NonEmpty.prepend v1 (controlPointDifferences scale p2 p3 remaining)

transformBy :: Transform2d tag (space @ units) -> BezierCurve2d (space @ units) -> BezierCurve2d (space @ units)
transformBy transform (BezierCurve2d controlPoints) =
  BezierCurve2d (NonEmpty.map (Point2d.transformBy transform) controlPoints)

instance Curve2d.Interface (BezierCurve2d (space @ units)) (space @ units) where
  startPointImpl (BezierCurve2d controlPoints) = NonEmpty.first controlPoints

  endPointImpl (BezierCurve2d controlPoints) = NonEmpty.last controlPoints

  pointOnImpl (BezierCurve2d controlPoints) t = deCasteljau t controlPoints

  segmentBoundsImpl (BezierCurve2d controlPoints) (Range a b) =
    Bounds2d.hullN (segmentControlPoints a b controlPoints)

  boundsImpl (BezierCurve2d controlPoints) = Bounds2d.hullN controlPoints

  derivativeImpl (BezierCurve2d (_ :| [])) = VectorCurve2d.zero
  derivativeImpl (BezierCurve2d (p1 :| p2 : rest)) = do
    let degree = 1 + List.length rest
    VectorCurve2d.bezierCurve (controlPointDifferences (Float.int degree) p1 p2 rest)

  reverseImpl (BezierCurve2d controlPoints) = BezierCurve2d (NonEmpty.reverse controlPoints)

  transformByImpl transform bezierCurve = Curve2d.new (transformBy transform bezierCurve)

  expressionImpl (BezierCurve2d controlPoints) = Just (expression controlPoints)

expression :: NonEmpty (Point2d (space @ units)) -> Expression Float (Point2d (space @ units))
expression controlPoints = expressionImpl (NonEmpty.map Expression.Curve2d.constant controlPoints)

expressionImpl ::
  NonEmpty (Expression Float (Point2d (space @ units))) ->
  Expression Float (Point2d (space @ units))
expressionImpl controlPoints = case controlPoints of
  point :| [] -> point
  _ :| NonEmpty rest -> expressionImpl (NonEmpty.map2 collapse controlPoints rest)

collapse ::
  Expression Float (Point2d (space @ units)) ->
  Expression Float (Point2d (space @ units)) ->
  Expression Float (Point2d (space @ units))
collapse p1 p2 = Expression.Curve2d.interpolateFrom p1 p2 Expression.parameter

{- | Construct a Bezier curve from its start point (first control point), inner control points and
end point (last control point). For example,

> BezierCurve2d.fromControlPoints p1 [ p2, p3 ] p4

will return a cubic spline with the given four control points.
-}
fromControlPoints ::
  Point2d (space @ units) ->
  List (Point2d (space @ units)) ->
  Point2d (space @ units) ->
  Curve2d (space @ units)
fromControlPoints startPoint innerControlPoints endPoint =
  Curve2d.new (BezierCurve2d (startPoint :| (innerControlPoints + [endPoint])))

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
hermite startCondition endCondition = Curve2d.new (hermiteBezier startCondition endCondition)

hermiteBezier ::
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  BezierCurve2d (space @ units)
hermiteBezier (startPoint, startDerivatives) (endPoint, endDerivatives) = do
  let numStartDerivatives = List.length startDerivatives
  let numEndDerivatives = List.length endDerivatives
  let curveDegree = 1 + numStartDerivatives + numEndDerivatives
  let scaledStartDerivatives = scaleDerivatives Positive 1.0 (Float.int curveDegree) startDerivatives
  let scaledEndDerivatives = scaleDerivatives Negative 1.0 (Float.int curveDegree) endDerivatives
  let startControlPoints = derivedControlPoints startPoint 1 (numStartDerivatives + 1) scaledStartDerivatives
  let endControlPoints = List.reverse (derivedControlPoints endPoint 1 (numEndDerivatives + 1) scaledEndDerivatives)
  let controlPoints = startPoint :| (startControlPoints + endControlPoints + [endPoint])
  BezierCurve2d controlPoints

scaleDerivatives :: Sign -> Float -> Float -> List (Vector2d (space @ units)) -> List (Vector2d (space @ units))
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
