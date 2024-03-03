module BezierCurve2d (fromControlPoints) where

import Bounds2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Float qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Range (Range (Range))
import Vector2d (Vector2d)
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
