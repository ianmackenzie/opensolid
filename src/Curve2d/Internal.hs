module Curve2d.Internal
  ( Curve2d (..)
  , IsCurve2d (..)
  , startPoint
  , endPoint
  , evaluateAt
  , segmentBounds
  , derivative
  )
where

import Angle (Angle)
import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve1d qualified
import Domain (Domain)
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Range qualified
import Units qualified
import Vector2d qualified
import VectorBox2d qualified
import VectorCurve2d (IsVectorCurve2d (..), VectorCurve2d (VectorCurve2d))
import VectorCurve2d qualified

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Line :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
  Arc :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
  Curve :: IsCurve2d curve (space @ units) => curve -> Curve2d (space @ units)

deriving instance Show (Curve2d coordinateSystem)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space')
  => Units.Coercion
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
  bisectImpl :: curve -> (curve, curve)
  boundingBoxImpl :: curve -> BoundingBox2d coordinateSystem

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint (Line p1 _) = p1
startPoint arc@(Arc{}) = evaluateAt 0.0 arc
startPoint (Curve curve) = startPointImpl curve

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint (Line _ p2) = p2
endPoint arc@(Arc{}) = evaluateAt 1.0 arc
endPoint (Curve curve) = endPointImpl curve

evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt t (Line p1 p2) = Point2d.interpolateFrom p1 p2 t
evaluateAt t (Arc p0 r a b) = let theta = Qty.interpolateFrom a b t in p0 + Vector2d.polar r theta
evaluateAt t (Curve curve) = Curve2d.Internal.evaluateAtImpl t curve

segmentBounds :: Domain -> Curve2d (space @ units) -> BoundingBox2d (space @ units)
segmentBounds t (Line p1 p2) =
  BoundingBox2d.hull2
    (Point2d.interpolateFrom p1 p2 t.minValue)
    (Point2d.interpolateFrom p1 p2 t.maxValue)
segmentBounds t (Arc p0 r a b) =
  let theta = a + t * (b - a) in p0 + VectorBox2d.polar (Range.constant r) theta
segmentBounds t (Curve curve) = Curve2d.Internal.segmentBoundsImpl t curve

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative (Line p1 p2) = VectorCurve2d.constant (p2 - p1)
derivative (Arc _ r a b) =
  let theta = a + Curve1d.parameter * (b - a)
      x = r * Curve1d.cos theta
      y = r * Curve1d.sin theta
   in VectorCurve2d.xy (Curve1d.derivative x) (Curve1d.derivative y)
derivative (Curve curve) = Curve2d.Internal.derivativeImpl curve

data PointCurveDifference (coordinateSystem :: CoordinateSystem)
  = PointCurveDifference (Point2d coordinateSystem) (Curve2d coordinateSystem)

instance IsVectorCurve2d (PointCurveDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (PointCurveDifference point curve) = point - evaluateAt t curve
  segmentBoundsImpl t (PointCurveDifference point curve) = point - segmentBounds t curve
  derivativeImpl (PointCurveDifference _ curve) = -(derivative curve)

instance
  (units ~ units', space ~ space')
  => Subtraction
      (Point2d (space @ units))
      (Curve2d (space' @ units'))
      (VectorCurve2d (space @ units))
  where
  point - curve = VectorCurve2d (PointCurveDifference point curve)

data CurvePointDifference (coordinateSystem :: CoordinateSystem)
  = CurvePointDifference (Curve2d coordinateSystem) (Point2d coordinateSystem)

instance IsVectorCurve2d (CurvePointDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (CurvePointDifference curve point) = evaluateAt t curve - point
  segmentBoundsImpl t (CurvePointDifference curve point) = segmentBounds t curve - point
  derivativeImpl (CurvePointDifference curve _) = derivative curve

instance
  (units ~ units', space ~ space')
  => Subtraction
      (Curve2d (space @ units))
      (Point2d (space' @ units'))
      (VectorCurve2d (space @ units))
  where
  curve - point = VectorCurve2d (CurvePointDifference curve point)
