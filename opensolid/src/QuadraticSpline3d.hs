module QuadraticSpline3d
  ( QuadraticSpline3d
  , fromControlPoints
  )
where

import Bounds3d qualified
import Curve3d (Curve3d (Curve3d))
import Curve3d qualified
import OpenSolid
import Point3d (Point3d (Point3d))
import Range (Range (Range))
import VectorCurve3d qualified

data QuadraticSpline3d (coordinateSystem :: CoordinateSystem) where
  QuadraticSpline3d ::
    Point3d (space @ units) ->
    Point3d (space @ units) ->
    Point3d (space @ units) ->
    QuadraticSpline3d (space @ units)

deriving instance Show (QuadraticSpline3d (space @ units))

blossom :: QuadraticSpline3d (space @ units) -> Float -> Float -> Point3d (space @ units)
blossom (QuadraticSpline3d (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3)) t1 t2 = do
  let r1 = 1 - t1
  let r2 = 1 - t2
  let s1 = r1 * r2
  let s2 = r1 * t2 + t1 * r2
  let s3 = t1 * t2
  let x = s1 * x1 + s2 * x2 + s3 * x3
  let y = s1 * y1 + s2 * y2 + s3 * y3
  let z = s1 * z1 + s2 * z2 + s3 * z3
  Point3d x y z

instance Curve3d.Interface (QuadraticSpline3d (space @ units)) (space @ units) where
  startPointImpl (QuadraticSpline3d p1 _ _) = p1

  endPointImpl (QuadraticSpline3d _ _ p3) = p3

  evaluateImpl spline t = blossom spline t t

  segmentBoundsImpl spline (Range tl th) =
    Bounds3d.hull3
      (blossom spline tl tl)
      (blossom spline tl th)
      (blossom spline th th)

  derivativeImpl (QuadraticSpline3d p1 p2 p3) =
    VectorCurve3d.line (2 * (p2 - p1)) (2 * (p3 - p2))

  reverseImpl (QuadraticSpline3d p1 p2 p3) = QuadraticSpline3d p3 p2 p1

  boundsImpl (QuadraticSpline3d p1 p2 p3) = Bounds3d.hull3 p1 p2 p3

fromControlPoints ::
  Tolerance units =>
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Result Curve3d.DegenerateCurve (Curve3d (space @ units))
fromControlPoints p1 p2 p3
  | p1 ~= p2 && p2 ~= p3 = Error Curve3d.DegenerateCurve
  | otherwise = Ok (Curve3d (QuadraticSpline3d p1 p2 p3))
