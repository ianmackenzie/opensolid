module QuadraticSpline2d
  ( QuadraticSpline2d
  , fromControlPoints
  )
where

import BezierCurve2d qualified
import Bounds2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
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
blossom (QuadraticSpline2d p1 p2 p3) t1 t2 = do
  let (x1, y1) = Point2d.coordinates p1
  let (x2, y2) = Point2d.coordinates p2
  let (x3, y3) = Point2d.coordinates p3
  let r1 = 1 - t1
  let r2 = 1 - t2
  let s1 = r1 * r2
  let s2 = r1 * t2 + t1 * r2
  let s3 = t1 * t2
  let x = s1 * x1 + s2 * x2 + s3 * x3
  let y = s1 * y1 + s2 * y2 + s3 * y3
  Point2d.xy x y

instance Curve2d.Interface (QuadraticSpline2d (space @ units)) (space @ units) where
  startPointImpl (QuadraticSpline2d p1 _ _) = p1

  endPointImpl (QuadraticSpline2d _ _ p3) = p3

  pointOnImpl spline t = blossom spline t t

  segmentBoundsImpl spline (Range tl th) =
    Bounds2d.hull3
      (blossom spline tl tl)
      (blossom spline tl th)
      (blossom spline th th)

  derivativeImpl (QuadraticSpline2d p1 p2 p3) =
    VectorCurve2d.line (2 * (p2 - p1)) (2 * (p3 - p2))

  reverseImpl (QuadraticSpline2d p1 p2 p3) = QuadraticSpline2d p3 p2 p1

  boundsImpl (QuadraticSpline2d p1 p2 p3) = Bounds2d.hull3 p1 p2 p3

  transformByImpl transform (QuadraticSpline2d p1 p2 p3) =
    Curve2d.new $
      QuadraticSpline2d
        (Point2d.transformBy transform p1)
        (Point2d.transformBy transform p2)
        (Point2d.transformBy transform p3)

  toAstImpl (QuadraticSpline2d p1 p2 p3) =
    Just (BezierCurve2d.toAst (NonEmpty.of3 p1 p2 p3))

fromControlPoints ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Curve2d (space @ units)
fromControlPoints p1 p2 p3 = Curve2d.new (QuadraticSpline2d p1 p2 p3)
