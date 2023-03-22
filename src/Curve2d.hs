module Curve2d
  ( Curve2d (Curve2d)
  , IsCurve2d (..)
  , startPoint
  , endPoint
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

import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve1d qualified
import Curve1d.Root qualified as Root
import List qualified
import OpenSolid
import Point2d (Point2d)
import Qty qualified
import Quadrature qualified
import Range (Range (..))
import Range qualified
import Result qualified
import Units (Unitless)
import Units qualified
import VectorBox2d qualified
import VectorCurve2d (IsVectorCurve2d, VectorCurve2d (VectorCurve2d))
import VectorCurve2d qualified

class IsCurve2d curve space units | curve -> space, curve -> units where
  startPointImpl :: curve -> Point2d (Coordinates space units)
  endPointImpl :: curve -> Point2d (Coordinates space units)
  pointOnImpl :: curve -> Float -> Point2d (Coordinates space units)
  segmentBoundsImpl :: curve -> Range Unitless -> BoundingBox2d (Coordinates space units)
  derivativeImpl :: curve -> VectorCurve2d (Coordinates space units)
  reverseImpl :: curve -> curve
  bisectImpl :: curve -> (curve, curve)
  boundingBoxImpl :: curve -> BoundingBox2d (Coordinates space units)

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Curve2d :: IsCurve2d curve space units => curve -> Curve2d (Coordinates space units)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space')
  => Units.Coercion
      units1
      units2
      (Curve2d (Coordinates space units1'))
      (Curve2d (Coordinates space' units2'))

startPoint :: Curve2d (Coordinates space units) -> Point2d (Coordinates space units)
startPoint (Curve2d curve) = startPointImpl curve

endPoint :: Curve2d (Coordinates space units) -> Point2d (Coordinates space units)
endPoint (Curve2d curve) = endPointImpl curve

pointOn :: Curve2d (Coordinates space units) -> Float -> Point2d (Coordinates space units)
pointOn (Curve2d curve) t = pointOnImpl curve t

segmentBounds :: Curve2d (Coordinates space units) -> Range Unitless -> BoundingBox2d (Coordinates space units)
segmentBounds (Curve2d curve) t = segmentBoundsImpl curve t

derivative :: Curve2d (Coordinates space units) -> VectorCurve2d (Coordinates space units)
derivative (Curve2d curve) = derivativeImpl curve

reverse :: Curve2d (Coordinates space units) -> Curve2d (Coordinates space units)
reverse (Curve2d curve) = Curve2d (reverseImpl curve)

bisect :: Curve2d (Coordinates space units) -> (Curve2d (Coordinates space units), Curve2d (Coordinates space units))
bisect (Curve2d curve) =
  let (curve1, curve2) = bisectImpl curve
   in (Curve2d curve1, Curve2d curve2)

boundingBox :: Curve2d (Coordinates space units) -> BoundingBox2d (Coordinates space units)
boundingBox (Curve2d curve) = boundingBoxImpl curve

instance IsCurve2d (Curve2d (Coordinates space units)) space units where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

instance IsCurve2d (Point2d (Coordinates space units)) space units where
  startPointImpl point = point
  endPointImpl point = point
  pointOnImpl point _ = point
  segmentBoundsImpl point _ = BoundingBox2d.constant point
  derivativeImpl _ = VectorCurve2d.zero
  reverseImpl = identity
  bisectImpl point = (point, point)
  boundingBoxImpl point = BoundingBox2d.constant point

data PointCurveDifference (coordinateSystem :: CoordinateSystem) = PointCurveDifference (Point2d coordinateSystem) (Curve2d coordinateSystem)

instance IsVectorCurve2d (PointCurveDifference (Coordinates space units)) space units where
  pointOn (PointCurveDifference point curve) t = point - pointOn curve t
  segmentBounds (PointCurveDifference point curve) t = point - segmentBounds curve t
  derivative (PointCurveDifference _ curve) = -(derivative curve)

instance (units ~ units', space ~ space') => Subtraction (Point2d (Coordinates space units)) (Curve2d (Coordinates space' units')) (VectorCurve2d (Coordinates space units)) where
  point - curve = VectorCurve2d (PointCurveDifference point curve)

data CurvePointDifference (coordinateSystem :: CoordinateSystem) = CurvePointDifference (Curve2d coordinateSystem) (Point2d coordinateSystem)

instance IsVectorCurve2d (CurvePointDifference (Coordinates space units)) space units where
  pointOn (CurvePointDifference curve point) t = pointOn curve t - point
  segmentBounds (CurvePointDifference curve point) t = segmentBounds curve t - point
  derivative (CurvePointDifference curve _) = derivative curve

instance (units ~ units', space ~ space') => Subtraction (Curve2d (Coordinates space units)) (Point2d (Coordinates space' units')) (VectorCurve2d (Coordinates space units)) where
  curve - point = VectorCurve2d (CurvePointDifference curve point)

data IsCoincidentWithPoint = IsCoincidentWithPoint deriving (Eq, Show)

instance IsError IsCoincidentWithPoint where
  errorMessage IsCoincidentWithPoint = "Curve is in fact a single point coincident with the given point"

passesThrough :: Tolerance units => Point2d (Coordinates space units) -> Curve2d (Coordinates space units) -> Bool
passesThrough point curve =
  Range.any (nearby point curve) (Range.from 0.0 1.0) |> Result.withDefault False

nearby :: Tolerance units => Point2d (Coordinates space units) -> Curve2d (Coordinates space units) -> Range Unitless -> Result Indeterminate Bool
nearby point curve domain
  | distance.minValue > ?tolerance = Ok False
  | distance.maxValue <= ?tolerance = Ok True
  | otherwise = Error Indeterminate
 where
  distance = VectorBox2d.magnitude (point - segmentBounds curve domain)

parameterValues :: Tolerance units => Point2d (Coordinates space units) -> Curve2d (Coordinates space units) -> Result IsCoincidentWithPoint (List Float)
parameterValues point curve = do
  let squaredDistanceFromCurve = VectorCurve2d.squaredMagnitude (Units.generalize (point - curve))
  roots <- Curve1d.roots squaredDistanceFromCurve ?? Error IsCoincidentWithPoint
  Ok (List.map Root.value roots)
 where
  ?tolerance = Qty.squared (Units.generalize ?tolerance)

overlappingSegments :: Tolerance units => Curve2d (Coordinates space units) -> Curve2d (Coordinates space units) -> List (Range Unitless)
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

samplingPoints :: Curve2d (Coordinates space units) -> Range Unitless -> List (Point2d (Coordinates space units))
samplingPoints curve domain =
  [pointOn curve (Range.interpolate domain t) | t <- Quadrature.points]

overlappingSegment :: Tolerance units => Curve2d (Coordinates space units) -> Curve2d (Coordinates space units) -> Range Unitless -> Bool
overlappingSegment curve1 curve2 domain1 =
  List.all [passesThrough point1 curve2 | point1 <- samplingPoints curve1 domain1]
