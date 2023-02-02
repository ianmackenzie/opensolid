module Curve2d
  ( Curve2d (Curve2d)
  , IsCurve2d (..)
  , passesThrough
  , find
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
import Range (Range (..))
import Range qualified
import VectorBox2d qualified
import VectorCurve2d (IsVectorCurve2d, VectorCurve2d (VectorCurve2d))
import VectorCurve2d qualified

class IsCurve2d curve where
  startPoint :: curve coordinates -> Point2d coordinates
  endPoint :: curve coordinates -> Point2d coordinates
  pointOn :: curve coordinates -> Float -> Point2d coordinates
  segmentBounds :: curve coordinates -> Range Unitless -> BoundingBox2d coordinates
  derivative :: curve coordinates -> VectorCurve2d Meters coordinates
  reverse :: curve coordinates -> curve coordinates
  bisect :: curve coordinates -> (curve coordinates, curve coordinates)
  boundingBox :: curve coordinates -> BoundingBox2d coordinates

data Curve2d coordinates = forall curve. IsCurve2d curve => Curve2d (curve coordinates)

instance IsCurve2d Curve2d where
  startPoint (Curve2d curve) = startPoint curve
  endPoint (Curve2d curve) = endPoint curve
  pointOn (Curve2d curve) t = pointOn curve t
  segmentBounds (Curve2d curve) t = segmentBounds curve t
  derivative (Curve2d curve) = derivative curve
  reverse (Curve2d curve) = Curve2d (reverse curve)
  bisect (Curve2d curve) =
    let (curve1, curve2) = bisect curve
     in (Curve2d curve1, Curve2d curve2)
  boundingBox (Curve2d curve) = boundingBox curve

instance IsCurve2d Point2d where
  startPoint point = point
  endPoint point = point
  pointOn point _ = point
  segmentBounds point _ = BoundingBox2d.constant point
  derivative _ = VectorCurve2d.zero
  reverse = identity
  bisect point = (point, point)
  boundingBox point = BoundingBox2d.constant point

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
  roots <- Curve1d.roots (VectorCurve2d.squaredMagnitude (curve - point)) ?! IsCoincidentWithPoint
  Ok (List.map Root.value roots)
  where
    ?tolerance = Qty.squared ?tolerance

_overlappingSegments :: Tolerance => Curve2d coordinates -> Curve2d coordinates -> List (Range Unitless)
_overlappingSegments curve1 curve2 =
  case (find (startPoint curve2) curve1, find (endPoint curve2) curve1) of
    -- curve1 is a single point coincident with the start point of curve2,
    -- so all of curve1 overlaps with curve2
    (Err IsCoincidentWithPoint, _) -> [Range.from 0.0 1.0]
    -- curve1 is a single point coincident with the end point of curve2,
    -- so all of curve1 overlaps with curve2
    (_, Err IsCoincidentWithPoint) -> [Range.from 0.0 1.0]
    -- Otherwise, check various segments bounded by curve endpoints,
    -- using the parameter values associated with those endpoints
    (Ok _startPoint2us, Ok _endPoint2us) ->
      let _u0 = [0.0 | passesThrough (startPoint curve1) curve2]
          _u1 = [1.0 | passesThrough (endPoint curve1) curve2]
          _uValues =
            List.concat [_u0, _u1, _startPoint2us, _endPoint2us]
              |> List.sort
              |> List.collapse (\u1 u2 -> if u1 == u2 then Just u1 else Nothing)
       in notImplemented
