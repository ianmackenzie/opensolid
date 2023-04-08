module QuadraticSpline2d
  ( QuadraticSpline2d
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , reverse
  , bisect
  , boundingBox
  , derivative
  , fromControlPoints
  , controlPoints
  )
where

import BoundingBox2d (BoundingBox2d (BoundingBox2d))
import BoundingBox2d qualified
import Curve1d qualified
import Curve2d (IsCurve2d (..))
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Range (Range (..))
import Range qualified
import Units (Unitless)
import VectorCurve2d (VectorCurve2d)

data QuadraticSpline2d (coordinateSystem :: CoordinateSystem)
  = QuadraticSpline2d
      (Point2d coordinateSystem)
      (Point2d coordinateSystem)
      (Point2d coordinateSystem)

startPoint :: QuadraticSpline2d (space @ units) -> Point2d (space @ units)
startPoint (QuadraticSpline2d p1 _ _) = p1

endPoint :: QuadraticSpline2d (space @ units) -> Point2d (space @ units)
endPoint (QuadraticSpline2d _ _ p3) = p3

pointOn :: QuadraticSpline2d (space @ units) -> Float -> Point2d (space @ units)
pointOn (QuadraticSpline2d p1 p2 p3) t =
  let q1 = Point2d.interpolateFrom p1 p2 t
      q2 = Point2d.interpolateFrom p2 p3 t
   in Point2d.interpolateFrom q1 q2 t

segmentBounds :: QuadraticSpline2d (space @ units) -> Range Unitless -> BoundingBox2d (space @ units)
segmentBounds spline (Range t1 t2) =
  let (QuadraticSpline2d a b c) = spline
      r1 = 1.0 - t1
      r2 = 1.0 - t2

      (Point2d ax ay) = a
      (Point2d bx by) = b
      (Point2d cx cy) = c

      ia = r1 * r1
      ib = t1 * r1
      ic = t1 * t1

      ja = r1 * r2
      jb = t1 * r2 + t2 * r1
      jc = t1 * t2

      ka = r2 * r2
      kb = t2 * r2
      kc = t2 * t2

      ix = ia * ax + ib * bx + ic * cx
      iy = ia * ay + ib * by + ic * cy

      jx = ja * ax + jb * bx + jc * cx
      jy = ja * ay + jb * by + jc * cy

      kx = ka * ax + kb * bx + kc * cx
      ky = ka * ay + kb * by + kc * cy
   in BoundingBox2d (Range.hull3 ix jx kx) (Range.hull3 iy jy ky)

derivative :: QuadraticSpline2d (space @ units) -> VectorCurve2d (space @ units)
derivative (QuadraticSpline2d p1 p2 p3) =
  let v1 = 2.0 * (p2 - p1)
      v2 = 2.0 * (p3 - p2)
   in v1 + Curve1d.parameter * (v2 - v1)

reverse :: QuadraticSpline2d (space @ units) -> QuadraticSpline2d (space @ units)
reverse (QuadraticSpline2d p1 p2 p3) = QuadraticSpline2d p3 p2 p1

bisect :: QuadraticSpline2d (space @ units) -> (QuadraticSpline2d (space @ units), QuadraticSpline2d (space @ units))
bisect (QuadraticSpline2d p1 p2 p3) =
  let q1 = Point2d.midpoint p1 p2
      q2 = Point2d.midpoint p2 p3
      r = Point2d.midpoint q1 q2
   in (QuadraticSpline2d p1 q1 r, QuadraticSpline2d r q2 p3)

boundingBox :: QuadraticSpline2d (space @ units) -> BoundingBox2d (space @ units)
boundingBox (QuadraticSpline2d p1 p2 p3) = BoundingBox2d.hull3 p1 p2 p3

instance IsCurve2d (QuadraticSpline2d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

fromControlPoints
  :: Point2d (space @ units)
  -> Point2d (space @ units)
  -> Point2d (space @ units)
  -> QuadraticSpline2d (space @ units)
fromControlPoints = QuadraticSpline2d

controlPoints
  :: QuadraticSpline2d (space @ units)
  -> ( Point2d (space @ units)
     , Point2d (space @ units)
     , Point2d (space @ units)
     )
controlPoints (QuadraticSpline2d p1 p2 p3) = (p1, p2, p3)
