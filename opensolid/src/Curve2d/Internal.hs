module Curve2d.Internal
  ( Curve2d (..)
  , Interface (..)
  , startPoint
  , endPoint
  , evaluateAt
  , segmentBounds
  , derivative
  , reverse
  , bounds
  )
where

import Bounds2d (Bounds2d)
import Bounds2d qualified
import Direction2d (Direction2d)
import DirectionCurve2d (DirectionCurve2d)
import DirectionCurve2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import U qualified
import Units qualified
import Vector2d qualified
import VectorBounds2d qualified
import VectorCurve2d (VectorCurve2d (VectorCurve2d))
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
    (Interface curve (space @ units)) =>
    curve ->
    DirectionCurve2d space ->
    Curve2d (space @ units)

deriving instance Show (Curve2d (space @ units))

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (Curve2d (space @ units1'))
    (Curve2d (space' @ units2'))

instance Interface (Curve2d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  evaluateAtImpl = evaluateAt
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  boundsImpl = bounds

instance
  (space ~ space', units ~ units') =>
  Intersects (Curve2d (space @ units)) (Point2d (space' @ units')) units
  where
  curve ^ point = Range.any (segmentIsCoincidentWithPoint point curve) U.domain

instance
  (space ~ space', units ~ units') =>
  Intersects (Point2d (space @ units)) (Curve2d (space' @ units')) units
  where
  point ^ curve = curve ^ point

segmentIsCoincidentWithPoint ::
  (Tolerance units) =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  U.Bounds ->
  Fuzzy Bool
segmentIsCoincidentWithPoint point curve domain
  | not (point ^ candidateBounds) = Resolved False
  | candidateBounds ~= point = Resolved True
  | otherwise = Unresolved
 where
  candidateBounds = segmentBounds domain curve

class
  (Show curve) =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  startPointImpl :: curve -> Point2d coordinateSystem
  endPointImpl :: curve -> Point2d coordinateSystem
  evaluateAtImpl :: Float -> curve -> Point2d coordinateSystem
  segmentBoundsImpl :: U.Bounds -> curve -> Bounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> curve
  boundsImpl :: curve -> Bounds2d coordinateSystem

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint (Line p1 _ _) = p1
startPoint arc@(Arc {}) = evaluateAt 0.0 arc
startPoint (Curve curve _) = startPointImpl curve

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint (Line _ p2 _) = p2
endPoint arc@(Arc {}) = evaluateAt 1.0 arc
endPoint (Curve curve _) = endPointImpl curve

evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt t (Line p1 p2 _) = Point2d.interpolateFrom p1 p2 t
evaluateAt t (Arc p0 r a b) = let theta = Qty.interpolateFrom a b t in p0 + Vector2d.polar r theta
evaluateAt t (Curve curve _) = evaluateAtImpl t curve

segmentBounds :: U.Bounds -> Curve2d (space @ units) -> Bounds2d (space @ units)
segmentBounds (Range t1 t2) (Line p1 p2 _) =
  Bounds2d.hull2 (Point2d.interpolateFrom p1 p2 t1) (Point2d.interpolateFrom p1 p2 t2)
segmentBounds t (Arc p0 r a b) =
  p0 + VectorBounds2d.polar (Range.constant r) (a + (b - a) * t)
segmentBounds t (Curve curve _) = segmentBoundsImpl t curve

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative (Line p1 p2 _) = VectorCurve2d.constant (p2 - p1)
derivative (Arc _ r a b) = VectorCurve2d.derivative (VectorCurve2d.arc r a b)
derivative (Curve curve _) = derivativeImpl curve

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse (Line p1 p2 direction) = Line p2 p1 -direction
reverse (Arc p0 r a b) = Arc p0 r b a
reverse (Curve curve tangentDirection) =
  Curve (reverseImpl curve) (DirectionCurve2d.reverse tangentDirection)

bounds :: Curve2d (space @ units) -> Bounds2d (space @ units)
bounds (Line p1 p2 _) = Bounds2d.hull2 p1 p2
bounds arc@(Arc {}) = segmentBounds U.domain arc
bounds (Curve curve _) = boundsImpl curve

data PointCurveDifference (coordinateSystem :: CoordinateSystem)
  = PointCurveDifference (Point2d coordinateSystem) (Curve2d coordinateSystem)

deriving instance Show (PointCurveDifference (space @ units))

instance VectorCurve2d.Interface (PointCurveDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (PointCurveDifference point curve) = point - evaluateAt t curve
  segmentBoundsImpl t (PointCurveDifference point curve) = point - segmentBounds t curve
  derivativeImpl (PointCurveDifference _ curve) = -(derivative curve)

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

deriving instance Show (CurvePointDifference (space @ units))

instance VectorCurve2d.Interface (CurvePointDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (CurvePointDifference curve point) = evaluateAt t curve - point
  segmentBoundsImpl t (CurvePointDifference curve point) = segmentBounds t curve - point
  derivativeImpl (CurvePointDifference curve _) = derivative curve

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (Curve2d (space @ units))
    (Point2d (space' @ units'))
    (VectorCurve2d (space @ units))
  where
  curve - point = VectorCurve2d (CurvePointDifference curve point)
