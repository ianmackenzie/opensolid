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
import Units (HasUnits, Unitless)
import Units qualified
import VectorBox2d qualified
import VectorCurve2d (IsVectorCurve2d, VectorCurve2d (VectorCurve2d))
import VectorCurve2d qualified

class IsCurve2d curve coordinates units | curve -> units, curve -> coordinates where
  startPointImpl :: curve -> Point2d coordinates units
  endPointImpl :: curve -> Point2d coordinates units
  pointOnImpl :: curve -> Float -> Point2d coordinates units
  segmentBoundsImpl :: curve -> Range Unitless -> BoundingBox2d coordinates units
  derivativeImpl :: curve -> VectorCurve2d coordinates units
  reverseImpl :: curve -> curve
  bisectImpl :: curve -> (curve, curve)
  boundingBoxImpl :: curve -> BoundingBox2d coordinates units

data Curve2d coordinates units = forall curve. IsCurve2d curve coordinates units => Curve2d curve

instance HasUnits (Curve2d coordinates)

startPoint :: Curve2d coordinates units -> Point2d coordinates units
startPoint (Curve2d curve) = startPointImpl curve

endPoint :: Curve2d coordinates units -> Point2d coordinates units
endPoint (Curve2d curve) = endPointImpl curve

pointOn :: Curve2d coordinates units -> Float -> Point2d coordinates units
pointOn (Curve2d curve) t = pointOnImpl curve t

segmentBounds :: Curve2d coordinates units -> Range Unitless -> BoundingBox2d coordinates units
segmentBounds (Curve2d curve) t = segmentBoundsImpl curve t

derivative :: Curve2d coordinates units -> VectorCurve2d coordinates units
derivative (Curve2d curve) = derivativeImpl curve

reverse :: Curve2d coordinates units -> Curve2d coordinates units
reverse (Curve2d curve) = Curve2d (reverseImpl curve)

bisect :: Curve2d coordinates units -> (Curve2d coordinates units, Curve2d coordinates units)
bisect (Curve2d curve) =
  let (curve1, curve2) = bisectImpl curve
   in (Curve2d curve1, Curve2d curve2)

boundingBox :: Curve2d coordinates units -> BoundingBox2d coordinates units
boundingBox (Curve2d curve) = boundingBoxImpl curve

instance IsCurve2d (Curve2d coordinates units) coordinates units where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

instance IsCurve2d (Point2d coordinates units) coordinates units where
  startPointImpl point = point
  endPointImpl point = point
  pointOnImpl point _ = point
  segmentBoundsImpl point _ = BoundingBox2d.constant point
  derivativeImpl _ = VectorCurve2d.zero
  reverseImpl = identity
  bisectImpl point = (point, point)
  boundingBoxImpl point = BoundingBox2d.constant point

data PointCurveDifference coordinates units = PointCurveDifference (Point2d coordinates units) (Curve2d coordinates units)

instance IsVectorCurve2d (PointCurveDifference coordinates units) coordinates units where
  pointOn (PointCurveDifference point curve) t = point - pointOn curve t
  segmentBounds (PointCurveDifference point curve) t = point - segmentBounds curve t
  derivative (PointCurveDifference _ curve) = -(derivative curve)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Point2d coordinates units) (Curve2d coordinates' units') (VectorCurve2d coordinates units) where
  point - curve = VectorCurve2d (PointCurveDifference point curve)

data CurvePointDifference coordinates units = CurvePointDifference (Curve2d coordinates units) (Point2d coordinates units)

instance IsVectorCurve2d (CurvePointDifference coordinates units) coordinates units where
  pointOn (CurvePointDifference curve point) t = pointOn curve t - point
  segmentBounds (CurvePointDifference curve point) t = segmentBounds curve t - point
  derivative (CurvePointDifference curve _) = derivative curve

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Curve2d coordinates units) (Point2d coordinates' units') (VectorCurve2d coordinates units) where
  curve - point = VectorCurve2d (CurvePointDifference curve point)

data IsCoincidentWithPoint = IsCoincidentWithPoint deriving (Eq, Show)

instance IsError IsCoincidentWithPoint where
  errorMessage IsCoincidentWithPoint = "Curve is in fact a single point coincident with the given point"

passesThrough :: Tolerance units => Point2d coordinates units -> Curve2d coordinates units -> Bool
passesThrough point curve =
  Range.any (nearby point curve) (Range.from 0.0 1.0) |> Result.withDefault False

nearby :: Tolerance units => Point2d coordinates units -> Curve2d coordinates units -> Range Unitless -> Result Indeterminate Bool
nearby point curve domain
  | distance.minValue > ?tolerance = Ok False
  | distance.maxValue <= ?tolerance = Ok True
  | otherwise = Error Indeterminate
 where
  distance = VectorBox2d.magnitude (point - segmentBounds curve domain)

parameterValues :: Tolerance units => Point2d coordinates units -> Curve2d coordinates units -> Result IsCoincidentWithPoint (List Float)
parameterValues point curve = do
  let squaredDistanceFromCurve = VectorCurve2d.squaredMagnitude (Units.generalize (point - curve))
  roots <- Curve1d.roots squaredDistanceFromCurve ?? Error IsCoincidentWithPoint
  Ok (List.map Root.value roots)
 where
  ?tolerance = Qty.squared (Units.generalize ?tolerance)

overlappingSegments :: Tolerance units => Curve2d coordinates units -> Curve2d coordinates units -> List (Range Unitless)
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

samplingPoints :: Curve2d coordinates units -> Range Unitless -> List (Point2d coordinates units)
samplingPoints curve domain =
  [pointOn curve (Range.interpolate domain t) | t <- Quadrature.points]

overlappingSegment :: Tolerance units => Curve2d coordinates units -> Curve2d coordinates units -> Range Unitless -> Bool
overlappingSegment curve1 curve2 domain1 =
  List.all [passesThrough point1 curve2 | point1 <- samplingPoints curve1 domain1]
