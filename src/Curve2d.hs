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
  , find
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
import VectorBox2d qualified
import VectorCurve2d (IsVectorCurve2d, VectorCurve2d (VectorCurve2d))
import VectorCurve2d qualified

class IsCurve2d curve coordinates | curve -> coordinates where
  startPointImpl :: curve -> Point2d coordinates
  endPointImpl :: curve -> Point2d coordinates
  pointOnImpl :: curve -> Float -> Point2d coordinates
  segmentBoundsImpl :: curve -> Range Unitless -> BoundingBox2d coordinates
  derivativeImpl :: curve -> VectorCurve2d Meters coordinates
  reverseImpl :: curve -> curve
  bisectImpl :: curve -> (curve, curve)
  boundingBoxImpl :: curve -> BoundingBox2d coordinates

data Curve2d coordinates = forall curve. IsCurve2d curve coordinates => Curve2d curve

startPoint :: Curve2d coordinates -> Point2d coordinates
startPoint (Curve2d curve) = startPointImpl curve

endPoint :: Curve2d coordinates -> Point2d coordinates
endPoint (Curve2d curve) = endPointImpl curve

pointOn :: Curve2d coordinates -> Float -> Point2d coordinates
pointOn (Curve2d curve) t = pointOnImpl curve t

segmentBounds :: Curve2d coordinates -> Range Unitless -> BoundingBox2d coordinates
segmentBounds (Curve2d curve) t = segmentBoundsImpl curve t

derivative :: Curve2d coordinates -> VectorCurve2d Meters coordinates
derivative (Curve2d curve) = derivativeImpl curve

reverse :: Curve2d coordinates -> Curve2d coordinates
reverse (Curve2d curve) = Curve2d (reverseImpl curve)

bisect :: Curve2d coordinates -> (Curve2d coordinates, Curve2d coordinates)
bisect (Curve2d curve) =
  let (curve1, curve2) = bisectImpl curve
   in (Curve2d curve1, Curve2d curve2)

boundingBox :: Curve2d coordinates -> BoundingBox2d coordinates
boundingBox (Curve2d curve) = boundingBoxImpl curve

instance IsCurve2d (Curve2d coordinates) coordinates where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  bisectImpl = bisect
  boundingBoxImpl = boundingBox

instance IsCurve2d (Point2d coordinates) coordinates where
  startPointImpl point = point
  endPointImpl point = point
  pointOnImpl point _ = point
  segmentBoundsImpl point _ = BoundingBox2d.constant point
  derivativeImpl _ = VectorCurve2d.zero
  reverseImpl = identity
  bisectImpl point = (point, point)
  boundingBoxImpl point = BoundingBox2d.constant point

instance (IsCurve2d x coordinates, IsCurve2d a coordinates', coordinates ~ coordinates') => IsCurve2d (Result x a) coordinates where
  startPointImpl (Err curve) = startPointImpl curve
  startPointImpl (Ok curve) = startPointImpl curve
  endPointImpl (Err curve) = endPointImpl curve
  endPointImpl (Ok curve) = endPointImpl curve
  pointOnImpl (Err curve) = pointOnImpl curve
  pointOnImpl (Ok curve) = pointOnImpl curve
  segmentBoundsImpl (Err curve) = segmentBoundsImpl curve
  segmentBoundsImpl (Ok curve) = segmentBoundsImpl curve
  derivativeImpl (Err curve) = derivativeImpl curve
  derivativeImpl (Ok curve) = derivativeImpl curve
  reverseImpl (Err curve) = let r = reverseImpl curve in Err r
  reverseImpl (Ok curve) = let r = reverseImpl curve in Ok r
  bisectImpl (Err curve) = let (c1, c2) = bisectImpl curve in (Err c1, Err c2)
  bisectImpl (Ok curve) = let (c1, c2) = bisectImpl curve in (Ok c1, Ok c2)
  boundingBoxImpl (Err curve) = boundingBoxImpl curve
  boundingBoxImpl (Ok curve) = boundingBoxImpl curve

data PointCurveDifference coordinates = PointCurveDifference (Point2d coordinates) (Curve2d coordinates)

instance IsVectorCurve2d PointCurveDifference Meters where
  pointOn (PointCurveDifference point curve) t = point - pointOn curve t
  segmentBounds (PointCurveDifference point curve) t = point - segmentBounds curve t
  derivative (PointCurveDifference _ curve) = -(derivative curve)

instance coordinates ~ coordinates' => Subtraction (Point2d coordinates) (Curve2d coordinates') (VectorCurve2d Meters coordinates) where
  point - curve = VectorCurve2d (PointCurveDifference point curve)

data CurvePointDifference coordinates = CurvePointDifference (Curve2d coordinates) (Point2d coordinates)

instance IsVectorCurve2d CurvePointDifference Meters where
  pointOn (CurvePointDifference curve point) t = pointOn curve t - point
  segmentBounds (CurvePointDifference curve point) t = segmentBounds curve t - point
  derivative (CurvePointDifference curve _) = derivative curve

instance coordinates ~ coordinates' => Subtraction (Curve2d coordinates) (Point2d coordinates') (VectorCurve2d Meters coordinates) where
  curve - point = VectorCurve2d (CurvePointDifference curve point)

data IsCoincidentWithPoint = IsCoincidentWithPoint deriving (Eq, Show)

passesThrough :: Tolerance => Point2d coordinates -> Curve2d coordinates -> Bool
passesThrough point curve =
  Range.any (nearby point curve) (Range.from 0.0 1.0) ?= False

nearby :: Tolerance => Point2d coordinates -> Curve2d coordinates -> Range Unitless -> Result Indeterminate Bool
nearby point curve domain
  | Range.minValue distance > ?tolerance = Ok False
  | Range.maxValue distance <= ?tolerance = Ok True
  | otherwise = Err Indeterminate
 where
  distance = VectorBox2d.magnitude (point - segmentBounds curve domain)

find :: Tolerance => Point2d coordinates -> Curve2d coordinates -> Result IsCoincidentWithPoint (List Float)
find point curve = do
  let squaredDistanceFromCurve = VectorCurve2d.squaredMagnitude (point - curve)
  roots <- Curve1d.roots squaredDistanceFromCurve ?! IsCoincidentWithPoint
  Ok (List.map Root.value roots)
 where
  ?tolerance = Qty.squared ?tolerance

overlappingSegments :: Tolerance => Curve2d coordinates -> Curve2d coordinates -> List (Range Unitless)
overlappingSegments curve1 curve2 =
  let u0 = [0.0 | passesThrough (startPoint curve1) curve2]
      u1 = [1.0 | passesThrough (endPoint curve1) curve2]
      start2us = find (startPoint curve2) curve1 ?= []
      end2us = find (endPoint curve2) curve1 ?= []
      candidateUValues = List.concat [u0, u1, start2us, end2us]
      uValues = List.sortAndDeduplicate candidateUValues
      candidateDomains = List.map2 Range.from uValues (List.drop 1 uValues)
   in List.filter (overlappingSegment curve1 curve2) candidateDomains

samplingPoints :: Curve2d coordinates -> Range Unitless -> List (Point2d coordinates)
samplingPoints curve domain =
  [pointOn curve (Range.interpolate domain t) | t <- Quadrature.points]

overlappingSegment :: Tolerance => Curve2d coordinates -> Curve2d coordinates -> Range Unitless -> Bool
overlappingSegment curve1 curve2 domain1 =
  List.all [passesThrough point1 curve2 | point1 <- samplingPoints curve1 domain1]
