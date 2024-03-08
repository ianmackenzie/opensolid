module BezierCurve2d
  ( fromControlPoints
  , extrapolant
  )
where

import Bounds2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Float qualified
import Int qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Range (Range (Range))
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
  NonEmpty.map (segmentControlPoint a b controlPoints) <|
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

instance Curve2d.Interface (BezierCurve2d (space @ units)) (space @ units) where
  startPointImpl (BezierCurve2d controlPoints) = NonEmpty.first controlPoints

  endPointImpl (BezierCurve2d controlPoints) = NonEmpty.last controlPoints

  evaluateAtImpl t (BezierCurve2d controlPoints) = deCasteljau t controlPoints

  segmentBoundsImpl (Range a b) (BezierCurve2d controlPoints) =
    Bounds2d.hullN (segmentControlPoints a b controlPoints)

  boundsImpl (BezierCurve2d controlPoints) = Bounds2d.hullN controlPoints

  derivativeImpl (BezierCurve2d (_ :| [])) = VectorCurve2d.zero
  derivativeImpl (BezierCurve2d (p1 :| p2 : rest)) = do
    let degree = 1 + List.length rest
    VectorCurve2d.bezierCurve (controlPointDifferences (Float.fromInt degree) p1 p2 rest)

  reverseImpl (BezierCurve2d controlPoints) = BezierCurve2d (NonEmpty.reverse controlPoints)

fromControlPoints ::
  Tolerance units =>
  NonEmpty (Point2d (space @ units)) ->
  Result Curve2d.DegenerateCurve (Curve2d (space @ units))
fromControlPoints controlPoints = Curve2d.from (BezierCurve2d controlPoints)

{- | Construct a Bezier curve with the given start point, start derivatives and end point.
For example,

> BezierCurve2d.extrapolant p1 [] p2

will result in a straight line (a linear spline) from @p1@ to @p2@, while

> BezierCurve2d.extrapolant p1 [ v1, v2 ] p2

will result in a cubic spline with:

  * Start point @p1@
  * First derivative at @p1@ equal to @v1@
  * Second derivative at @p1@ equal to @v2@
  * End point @p2@
-}
extrapolant ::
  Tolerance units =>
  Point2d (space @ units) ->
  List (Vector2d (space @ units)) ->
  Point2d (space @ units) ->
  Result Curve2d.DegenerateCurve (Curve2d (space @ units))
extrapolant startPoint startDerivatives endPoint = do
  let n = 1 + List.length startDerivatives
  let qs = scaledDerivatives 1.0 (Float.fromInt n) startDerivatives
  let controlPoints = startPoint :| followingControlPoints startPoint 1 n qs endPoint
  fromControlPoints controlPoints

scaledDerivatives :: Float -> Float -> List (Vector2d (space @ units)) -> List (Vector2d (space @ units))
scaledDerivatives _ _ [] = []
scaledDerivatives s n (first : rest) = do
  let s' = s / n
  let n' = n - 1.0
  s' * first : scaledDerivatives s' n' rest

offset :: Int -> List (Vector2d (space @ units)) -> Vector2d (space @ units)
offset i qs =
  List.take i qs
    |> List.mapWithIndex (\j q -> Float.fromInt (Int.choose (i - 1) j) * q)
    |> Vector2d.sum

followingControlPoints ::
  Point2d (space @ units) ->
  Int ->
  Int ->
  List (Vector2d (space @ units)) ->
  Point2d (space @ units) ->
  List (Point2d (space @ units))
followingControlPoints previousPoint i n qs endPoint
  | i < n = do
      let newPoint = previousPoint + offset i qs
      newPoint : followingControlPoints newPoint (i + 1) n qs endPoint
  | otherwise = [endPoint]
