module CubicSpline2d (fromControlPoints) where

import Arithmetic.Unboxed
import BezierCurve2d qualified
import Bounds2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d (Point2d#))
import Point2d qualified
import Qty (Qty (Qty#))
import Range (Range (Range))
import VectorCurve2d qualified

data CubicSpline2d (coordinateSystem :: CoordinateSystem) where
  CubicSpline2d ::
    Point2d (space @ units) ->
    Point2d (space @ units) ->
    Point2d (space @ units) ->
    Point2d (space @ units) ->
    CubicSpline2d (space @ units)

deriving instance Show (CubicSpline2d (space @ units))

blossom :: CubicSpline2d (space @ units) -> Float -> Float -> Float -> Point2d (space @ units)
blossom (CubicSpline2d p1 p2 p3 p4) (Qty# t1#) (Qty# t2#) (Qty# t3#) = do
  let !(Point2d# x1# y1#) = p1
  let !(Point2d# x2# y2#) = p2
  let !(Point2d# x3# y3#) = p3
  let !(Point2d# x4# y4#) = p4
  let r1# = 1.0## -# t1#
  let r2# = 1.0## -# t2#
  let r3# = 1.0## -# t3#
  let s1# = r1# *# r2# *# r3#
  let s2# = r1# *# r2# *# t3# +# r1# *# t2# *# r3# +# t1# *# r2# *# r3#
  let s3# = t1# *# t2# *# r3# +# t1# *# r2# *# t3# +# r1# *# t2# *# t3#
  let s4# = t1# *# t2# *# t3#
  let x# = s1# *# x1# +# s2# *# x2# +# s3# *# x3# +# s4# *# x4#
  let y# = s1# *# y1# +# s2# *# y2# +# s3# *# y3# +# s4# *# y4#
  Point2d.xy (Qty# x#) (Qty# y#)

instance Curve2d.Interface (CubicSpline2d (space @ units)) (space @ units) where
  startPointImpl (CubicSpline2d p1 _ _ _) = p1

  endPointImpl (CubicSpline2d _ _ _ p4) = p4

  pointOnImpl spline t = blossom spline t t t

  segmentBoundsImpl spline (Range tl th) =
    Bounds2d.hull4
      (blossom spline tl tl tl)
      (blossom spline tl tl th)
      (blossom spline tl th th)
      (blossom spline th th th)

  derivativeImpl (CubicSpline2d p1 p2 p3 p4) =
    VectorCurve2d.quadraticSpline
      (3 * (p2 - p1))
      (3 * (p3 - p2))
      (3 * (p4 - p3))

  reverseImpl (CubicSpline2d p1 p2 p3 p4) = CubicSpline2d p4 p3 p2 p1

  boundsImpl (CubicSpline2d p1 p2 p3 p4) = Bounds2d.hull4 p1 p2 p3 p4

  transformByImpl transform (CubicSpline2d p1 p2 p3 p4) =
    Curve2d.new $
      CubicSpline2d
        (Point2d.transformBy transform p1)
        (Point2d.transformBy transform p2)
        (Point2d.transformBy transform p3)
        (Point2d.transformBy transform p4)

  toAstImpl (CubicSpline2d p1 p2 p3 p4) =
    Just (BezierCurve2d.toAst (NonEmpty.of4 p1 p2 p3 p4))

fromControlPoints ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Curve2d (space @ units)
fromControlPoints p1 p2 p3 p4 = Curve2d.new (CubicSpline2d p1 p2 p3 p4)
