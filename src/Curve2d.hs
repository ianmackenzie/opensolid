module Curve2d
  ( Curve2d (Curve2d, Line, Arc)
  , IsCurve2d (..)
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , derivative
  , reverse
  , bisect
  , boundingBox
  , passesThrough
  , parameterValues
  , overlappingSegments
  )
where

import Angle (Angle)
import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve1d qualified
import Curve1d.Root qualified as Root
import List qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Quadrature qualified
import Range (Range (..))
import Range qualified
import Result qualified
import Units (Unitless)
import Units qualified
import Vector2d qualified
import VectorBox2d qualified
import VectorCurve2d (IsVectorCurve2d, VectorCurve2d (VectorCurve2d))
import VectorCurve2d qualified

class IsCurve2d curve (coordinateSystem :: CoordinateSystem) | curve -> coordinateSystem where
  startPointImpl :: curve -> Point2d coordinateSystem
  endPointImpl :: curve -> Point2d coordinateSystem
  pointOnImpl :: curve -> Float -> Point2d coordinateSystem
  segmentBoundsImpl :: curve -> Range Unitless -> BoundingBox2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> curve
  bisectImpl :: curve -> (curve, curve)
  boundingBoxImpl :: curve -> BoundingBox2d coordinateSystem

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Line :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
  Arc :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
  Curve2d :: IsCurve2d curve (space @ units) => curve -> Curve2d (space @ units)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space')
  => Units.Coercion
      units1
      units2
      (Curve2d (space @ units1'))
      (Curve2d (space' @ units2'))

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint (Line p1 _) = p1
startPoint arc@(Arc{}) = pointOn arc 0.0
startPoint (Curve2d curve) = startPointImpl curve

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint (Line _ p2) = p2
endPoint arc@(Arc{}) = pointOn arc 1.0
endPoint (Curve2d curve) = endPointImpl curve

pointOn :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
pointOn (Line p1 p2) t = Point2d.interpolateFrom p1 p2 t
pointOn (Arc p0 r a b) t = let theta = Qty.interpolateFrom a b t in p0 + Vector2d.polar r theta
pointOn (Curve2d curve) t = pointOnImpl curve t

segmentBounds :: Curve2d (space @ units) -> Range Unitless -> BoundingBox2d (space @ units)
segmentBounds (Line p1 p2) t =
  BoundingBox2d.hull2
    (Point2d.interpolateFrom p1 p2 t.minValue)
    (Point2d.interpolateFrom p1 p2 t.maxValue)
segmentBounds (Arc p0 r a b) t =
  let theta = a + t * (b - a) in p0 + VectorBox2d.polar (Range.constant r) theta
segmentBounds (Curve2d curve) t = segmentBoundsImpl curve t

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative (Line p1 p2) = VectorCurve2d.constant (p2 - p1)
derivative (Arc _ r a b) =
  let theta = a + Curve1d.parameter * (b - a)
      x = r * Curve1d.cos theta
      y = r * Curve1d.sin theta
   in VectorCurve2d.xy (Curve1d.derivative x) (Curve1d.derivative y)
derivative (Curve2d curve) = derivativeImpl curve

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse (Line p1 p2) = Line p2 p1
reverse (Arc p0 r a b) = Arc p0 r b a
reverse (Curve2d curve) = Curve2d (reverseImpl curve)

bisect :: Curve2d (space @ units) -> (Curve2d (space @ units), Curve2d (space @ units))
bisect (Line p1 p2) = let mid = Point2d.midpoint p1 p2 in (Line p1 mid, Line mid p2)
bisect (Arc p0 r a b) = let mid = Qty.midpoint a b in (Arc p0 r a mid, Arc p0 r mid b)
bisect (Curve2d curve) =
  let (curve1, curve2) = bisectImpl curve
   in (Curve2d curve1, Curve2d curve2)

boundingBox :: Curve2d (space @ units) -> BoundingBox2d (space @ units)
boundingBox (Line p1 p2) = BoundingBox2d.hull2 p1 p2
boundingBox arc@(Arc{}) = segmentBounds arc Range.unit
boundingBox (Curve2d curve) = boundingBoxImpl curve

instance IsCurve2d (Curve2d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

instance IsCurve2d (Point2d (space @ units)) (space @ units) where
  startPointImpl point = point
  endPointImpl point = point
  pointOnImpl point _ = point
  segmentBoundsImpl point _ = BoundingBox2d.constant point
  derivativeImpl _ = VectorCurve2d.zero
  reverseImpl = identity
  bisectImpl point = (point, point)
  boundingBoxImpl point = BoundingBox2d.constant point

data PointCurveDifference (coordinateSystem :: CoordinateSystem) = PointCurveDifference (Point2d coordinateSystem) (Curve2d coordinateSystem)

instance IsVectorCurve2d (PointCurveDifference (space @ units)) (space @ units) where
  pointOn (PointCurveDifference point curve) t = point - pointOn curve t
  segmentBounds (PointCurveDifference point curve) t = point - segmentBounds curve t
  derivative (PointCurveDifference _ curve) = -(derivative curve)

instance (units ~ units', space ~ space') => Subtraction (Point2d (space @ units)) (Curve2d (space' @ units')) (VectorCurve2d (space @ units)) where
  point - curve = VectorCurve2d (PointCurveDifference point curve)

data CurvePointDifference (coordinateSystem :: CoordinateSystem) = CurvePointDifference (Curve2d coordinateSystem) (Point2d coordinateSystem)

instance IsVectorCurve2d (CurvePointDifference (space @ units)) (space @ units) where
  pointOn (CurvePointDifference curve point) t = pointOn curve t - point
  segmentBounds (CurvePointDifference curve point) t = segmentBounds curve t - point
  derivative (CurvePointDifference curve _) = derivative curve

instance (units ~ units', space ~ space') => Subtraction (Curve2d (space @ units)) (Point2d (space' @ units')) (VectorCurve2d (space @ units)) where
  curve - point = VectorCurve2d (CurvePointDifference curve point)

data IsCoincidentWithPoint = IsCoincidentWithPoint deriving (Eq, Show)

instance IsError IsCoincidentWithPoint where
  errorMessage IsCoincidentWithPoint = "Curve is in fact a single point coincident with the given point"

passesThrough :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units) -> Bool
passesThrough point curve =
  Range.any (nearby point curve) (Range.from 0.0 1.0) |> Result.withDefault False

nearby :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units) -> Range Unitless -> Result Indeterminate Bool
nearby point curve domain
  | distance.minValue > ?tolerance = Ok False
  | distance.maxValue <= ?tolerance = Ok True
  | otherwise = Error Indeterminate
 where
  distance = VectorBox2d.magnitude (point - segmentBounds curve domain)

parameterValues :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units) -> Result IsCoincidentWithPoint (List Float)
parameterValues point curve = do
  let squaredDistanceFromCurve = VectorCurve2d.squaredMagnitude (Units.generalize (point - curve))
  roots <- Curve1d.roots squaredDistanceFromCurve ?? Error IsCoincidentWithPoint
  Ok (List.map Root.value roots)
 where
  ?tolerance = Qty.squared (Units.generalize ?tolerance)

overlappingSegments :: Tolerance units => Curve2d (space @ units) -> Curve2d (space @ units) -> List (Range Unitless)
overlappingSegments curve1 curve2 =
  let segmentEndpoints =
        List.sortAndDeduplicate $
          List.concat
            [ [0.0 | passesThrough (startPoint curve1) curve2]
            , [1.0 | passesThrough (endPoint curve1) curve2]
            , parameterValues (startPoint curve2) curve1 |> Result.withDefault []
            , parameterValues (endPoint curve2) curve1 |> Result.withDefault []
            ]
      candidateDomains = List.successive Range.from segmentEndpoints
   in List.filter (overlappingSegment curve1 curve2) candidateDomains

samplingPoints :: Curve2d (space @ units) -> Range Unitless -> List (Point2d (space @ units))
samplingPoints curve domain =
  [pointOn curve (Range.interpolate domain t) | t <- Quadrature.points]

overlappingSegment :: Tolerance units => Curve2d (space @ units) -> Curve2d (space @ units) -> Range Unitless -> Bool
overlappingSegment curve1 curve2 domain1 =
  List.all [passesThrough point1 curve2 | point1 <- samplingPoints curve1 domain1]
