module QuadraticSpline3d
  ( QuadraticSpline3d
  , fromControlPoints
  )
where

import BoundingBox3d qualified
import Curve3d (Curve3d (Curve3d), DegenerateCurve (DegenerateCurve), IsCurve3d (..))
import OpenSolid
import Point3d (Point3d (Point3d))
import Range (Range (Range))
import VectorCurve3d qualified

data QuadraticSpline3d (coordinateSystem :: CoordinateSystem)
  = QuadraticSpline3d
      (Point3d coordinateSystem)
      (Point3d coordinateSystem)
      (Point3d coordinateSystem)
  deriving (Show)

blossom :: QuadraticSpline3d (space @ units) -> Float -> Float -> Point3d (space @ units)
blossom (QuadraticSpline3d (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3)) t1 t2 =
  let r1 = 1.0 - t1
      r2 = 1.0 - t2
      s1 = r1 * r2
      s2 = r1 * t2 + t1 * r2
      s3 = t1 * t2
      x = s1 * x1 + s2 * x2 + s3 * x3
      y = s1 * y1 + s2 * y2 + s3 * y3
      z = s1 * z1 + s2 * z2 + s3 * z3
   in Point3d x y z

instance IsCurve3d (QuadraticSpline3d (space @ units)) (space @ units) where
  startPointImpl (QuadraticSpline3d p1 _ _) = p1

  endPointImpl (QuadraticSpline3d _ _ p3) = p3

  evaluateImpl spline t = blossom spline t t

  segmentBoundsImpl spline (Range tl th) =
    BoundingBox3d.hull3
      (blossom spline tl tl)
      (blossom spline tl th)
      (blossom spline th th)

  derivativeImpl (QuadraticSpline3d p1 p2 p3) =
    VectorCurve3d.line (2.0 * (p2 - p1)) (2.0 * (p3 - p2))

  reverseImpl (QuadraticSpline3d p1 p2 p3) = QuadraticSpline3d p3 p2 p1

  boundingBoxImpl (QuadraticSpline3d p1 p2 p3) = BoundingBox3d.hull3 p1 p2 p3

fromControlPoints ::
  Tolerance units =>
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Result Curve3d.DegenerateCurve (Curve3d (space @ units))
fromControlPoints p1 p2 p3
  | p1 ~= p2 && p2 ~= p3 = Error DegenerateCurve
  | otherwise = Ok (Curve3d (QuadraticSpline3d p1 p2 p3))
