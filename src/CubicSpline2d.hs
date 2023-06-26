module CubicSpline2d
  ( CubicSpline2d
  , fromControlPoints
  )
where

import BoundingBox2d qualified
import Curve2d (Curve2d, DegenerateCurve, IsCurve2d (..))
import Curve2d qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Range (Range (..))
import VectorCurve2d qualified

data CubicSpline2d (coordinateSystem :: CoordinateSystem)
  = CubicSpline2d
      (Point2d coordinateSystem)
      (Point2d coordinateSystem)
      (Point2d coordinateSystem)
      (Point2d coordinateSystem)
  deriving (Show)

blossom :: CubicSpline2d (space @ units) -> Float -> Float -> Float -> Point2d (space @ units)
blossom (CubicSpline2d (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) (Point2d x4 y4)) t1 t2 t3 =
  let r1 = 1.0 - t1
      r2 = 1.0 - t2
      r3 = 1.0 - t3
      s1 = r1 * r2 * r3
      s2 = r1 * r2 * t3 + r1 * t2 * r3 + t1 * r2 * r3
      s3 = t1 * t2 * r3 + t1 * r2 * t3 + r1 * t2 * t3
      s4 = t1 * t2 * t3
      x = s1 * x1 + s2 * x2 + s3 * x3 + s4 * x4
      y = s1 * y1 + s2 * y2 + s3 * y3 + s4 * y4
   in Point2d x y

instance IsCurve2d (CubicSpline2d (space @ units)) (space @ units) where
  startPointImpl (CubicSpline2d p1 _ _ _) = p1

  endPointImpl (CubicSpline2d _ _ _ p4) = p4

  evaluateAtImpl t spline = blossom spline t t t

  segmentBoundsImpl (Range tl th) spline =
    BoundingBox2d.hull4
      (blossom spline tl tl tl)
      (blossom spline tl tl th)
      (blossom spline tl th th)
      (blossom spline th th th)

  derivativeImpl (CubicSpline2d p1 p2 p3 p4) =
    VectorCurve2d.quadraticSpline
      (3.0 * (p2 - p1))
      (3.0 * (p3 - p2))
      (3.0 * (p4 - p3))

  reverseImpl (CubicSpline2d p1 p2 p3 p4) = CubicSpline2d p4 p3 p2 p1

  bisectImpl spline =
    let l1 = startPointImpl spline
        l2 = blossom spline 0.0 0.0 0.5
        l3 = blossom spline 0.0 0.5 0.5
        mid = blossom spline 0.5 0.5 0.5
        r2 = blossom spline 0.5 0.5 1.0
        r3 = blossom spline 0.5 1.0 1.0
        r4 = endPointImpl spline
     in (CubicSpline2d l1 l2 l3 mid, CubicSpline2d mid r2 r3 r4)

  boundingBoxImpl (CubicSpline2d p1 p2 p3 p4) = BoundingBox2d.hull4 p1 p2 p3 p4

fromControlPoints
  :: Tolerance units
  => Point2d (space @ units)
  -> Point2d (space @ units)
  -> Point2d (space @ units)
  -> Point2d (space @ units)
  -> Result DegenerateCurve (Curve2d (space @ units))
fromControlPoints p1 p2 p3 p4 = Curve2d.from (CubicSpline2d p1 p2 p3 p4)
