module QuadraticSpline2d
  ( QuadraticSpline2d
  , fromControlPoints
  )
where

import Bounds2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Range (Range (Range))
import VectorCurve2d qualified

data QuadraticSpline2d (coordinateSystem :: CoordinateSystem) where
  QuadraticSpline2d ::
    Point2d (space @ units) ->
    Point2d (space @ units) ->
    Point2d (space @ units) ->
    QuadraticSpline2d (space @ units)

deriving instance Show (QuadraticSpline2d (space @ units))

blossom :: QuadraticSpline2d (space @ units) -> Float -> Float -> Point2d (space @ units)
blossom (QuadraticSpline2d (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3)) t1 t2 =
  let r1 = 1.0 - t1
      r2 = 1.0 - t2
      s1 = r1 * r2
      s2 = r1 * t2 + t1 * r2
      s3 = t1 * t2
      x = s1 * x1 + s2 * x2 + s3 * x3
      y = s1 * y1 + s2 * y2 + s3 * y3
   in Point2d x y

instance Curve2d.Interface (QuadraticSpline2d (space @ units)) (space @ units) where
  startPointImpl (QuadraticSpline2d p1 _ _) = p1

  endPointImpl (QuadraticSpline2d _ _ p3) = p3

  evaluateAtImpl t spline = blossom spline t t

  segmentBoundsImpl (Range tl th) spline =
    Bounds2d.hull3
      (blossom spline tl tl)
      (blossom spline tl th)
      (blossom spline th th)

  derivativeImpl (QuadraticSpline2d p1 p2 p3) =
    VectorCurve2d.line (2.0 * (p2 - p1)) (2.0 * (p3 - p2))

  reverseImpl (QuadraticSpline2d p1 p2 p3) = QuadraticSpline2d p3 p2 p1

  boundsImpl (QuadraticSpline2d p1 p2 p3) = Bounds2d.hull3 p1 p2 p3

fromControlPoints ::
  Tolerance units =>
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Result Curve2d.DegenerateCurve (Curve2d (space @ units))
fromControlPoints p1 p2 p3 = Curve2d.wrap (QuadraticSpline2d p1 p2 p3)
