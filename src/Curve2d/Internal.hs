module Curve2d.Internal
  ( Curve2d (..)
  , IsCurve2d (..)
  , startPoint
  , endPoint
  , evaluateAt
  , segmentBounds
  , derivative
  , reverse
  )
where

import Angle (Angle)
import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Direction2d (Direction2d)
import Domain (Domain)
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Units qualified
import Vector2d qualified
import VectorBox2d qualified
import VectorCurve2d (IsVectorCurve2d (..), VectorCurve2d (VectorCurve2d))
import VectorCurve2d qualified

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Line ::
    Point2d (space @ units) ->
    Point2d (space @ units) ->
    Direction2d space ->
    Curve2d (space @ units)
  Arc ::
    Point2d (space @ units) ->
    Qty units ->
    Angle ->
    Angle ->
    Curve2d (space @ units)
  Curve ::
    IsCurve2d curve (space @ units) =>
    curve ->
    Qty units ->
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units) ->
    Curve2d (space @ units)

deriving instance Show (Curve2d coordinateSystem)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (Curve2d (space @ units1'))
    (Curve2d (space' @ units2'))

class
  Show curve =>
  IsCurve2d curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  startPointImpl :: curve -> Point2d coordinateSystem
  endPointImpl :: curve -> Point2d coordinateSystem
  evaluateAtImpl :: Float -> curve -> Point2d coordinateSystem
  segmentBoundsImpl :: Domain -> curve -> BoundingBox2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> curve
  boundingBoxImpl :: curve -> BoundingBox2d coordinateSystem

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint (Line p1 _ _) = p1
startPoint arc@(Arc{}) = evaluateAt 0.0 arc
startPoint (Curve curve _ _ _) = startPointImpl curve

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint (Line _ p2 _) = p2
endPoint arc@(Arc{}) = evaluateAt 1.0 arc
endPoint (Curve curve _ _ _) = endPointImpl curve

evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt t (Line p1 p2 _) = Point2d.interpolateFrom p1 p2 t
evaluateAt t (Arc p0 r a b) = let theta = Qty.interpolateFrom a b t in p0 + Vector2d.polar r theta
evaluateAt t (Curve curve _ _ _) = Curve2d.Internal.evaluateAtImpl t curve

segmentBounds :: Domain -> Curve2d (space @ units) -> BoundingBox2d (space @ units)
segmentBounds (Range t1 t2) (Line p1 p2 _) =
  BoundingBox2d.hull2 (Point2d.interpolateFrom p1 p2 t1) (Point2d.interpolateFrom p1 p2 t2)
segmentBounds t (Arc p0 r a b) =
  p0 + VectorBox2d.polar (Range.constant r) (a + (b - a) * t)
segmentBounds t (Curve curve _ _ _) =
  Curve2d.Internal.segmentBoundsImpl t curve

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative (Line p1 p2 _) = VectorCurve2d.constant (p2 - p1)
derivative (Arc _ r a b) = VectorCurve2d.derivative (VectorCurve2d.arc r a b)
derivative (Curve _ _ first _) = first

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse (Line p1 p2 direction) = Line p2 p1 -direction
reverse (Arc p0 r a b) = Arc p0 r b a
reverse (Curve curve tolerance first second) =
  Curve
    (Curve2d.Internal.reverseImpl curve)
    tolerance
    -(VectorCurve2d.reverse first)
    -(VectorCurve2d.reverse second)

data PointCurveDifference (coordinateSystem :: CoordinateSystem)
  = PointCurveDifference (Point2d coordinateSystem) (Curve2d coordinateSystem)

deriving instance Show (PointCurveDifference coordinateSystem)

instance IsVectorCurve2d (PointCurveDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (PointCurveDifference point curve) = point - evaluateAt t curve
  segmentBoundsImpl t (PointCurveDifference point curve) = point - segmentBounds t curve
  derivativeImpl (PointCurveDifference _ curve) = -(derivative curve)
  reverseImpl (PointCurveDifference point curve) = PointCurveDifference point (reverse curve)

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (Point2d (space @ units))
    (Curve2d (space' @ units'))
    (VectorCurve2d (space @ units))
  where
  point - curve = VectorCurve2d (PointCurveDifference point curve)

data CurvePointDifference (coordinateSystem :: CoordinateSystem)
  = CurvePointDifference (Curve2d coordinateSystem) (Point2d coordinateSystem)

deriving instance Show (CurvePointDifference coordinateSystem)

instance IsVectorCurve2d (CurvePointDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (CurvePointDifference curve point) = evaluateAt t curve - point
  segmentBoundsImpl t (CurvePointDifference curve point) = segmentBounds t curve - point
  derivativeImpl (CurvePointDifference curve _) = derivative curve
  reverseImpl (CurvePointDifference curve point) = CurvePointDifference (reverse curve) point

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (Curve2d (space @ units))
    (Point2d (space' @ units'))
    (VectorCurve2d (space @ units))
  where
  curve - point = VectorCurve2d (CurvePointDifference curve point)
