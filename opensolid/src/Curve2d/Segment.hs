module Curve2d.Segment
  ( Segment
  , init
  , overlaps
  , isEndpointIntersectionCandidate
  , endpointIntersectionResolved
  , isTangentIntersectionCandidate
  , tangentIntersectionSign
  , findTangentIntersection
  , isCrossingIntersectionCandidate
  , crossingIntersectionSign
  , findCrossingIntersection
  )
where

import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import {-# SOURCE #-} Curve2d (Curve2d)
import {-# SOURCE #-} Curve2d qualified
import Curve2d.Derivatives (Derivatives)
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import DirectionBox2d (DirectionBox2d)
import DirectionCurve2d qualified
import Domain (Domain)
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified
import Units ((:/))
import Units qualified
import VectorBox2d (VectorBox2d)
import VectorBox2d qualified
import VectorCurve2d qualified

data Segment (coordinateSystem :: CoordinateSystem) where
  Segment ::
    BoundingBox2d (space @ units) ->
    ~(VectorBox2d (space @ units)) ->
    ~(VectorBox2d (space @ units)) ->
    ~(DirectionBox2d space) ->
    Segment (space @ units)

init ::
  (Tolerance units) =>
  Derivatives (space @ units) ->
  Domain ->
  Segment (space @ units)
init derivatives domain =
  let curveBounds = Curve2d.segmentBounds domain derivatives.curve
      firstBounds = VectorCurve2d.segmentBounds domain derivatives.first
      secondBounds = VectorCurve2d.segmentBounds domain derivatives.second
      tangentBounds = DirectionCurve2d.segmentBounds domain (Curve2d.tangentDirection derivatives.curve)
   in Segment curveBounds firstBounds secondBounds tangentBounds

overlaps :: (Tolerance units) => Segment (space @ units) -> Segment (space @ units) -> Bool
overlaps (Segment firstBounds _ _ _) (Segment secondBounds _ _ _) =
  Range.minValue (VectorBox2d.magnitude (firstBounds - secondBounds)) <= ?tolerance

crossProductResolution :: Segment (space @ units) -> Segment (space @ units) -> Float
crossProductResolution (Segment _ _ _ tangentBounds1) (Segment _ _ _ tangentBounds2) =
  Range.resolution (tangentBounds1 >< tangentBounds2)

isEndpointIntersectionCandidate ::
  (Float, Float) ->
  Domain ->
  Domain ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Bool
isEndpointIntersectionCandidate (u0, v0) u v _ _ =
  Range.includes u0 u && Range.includes v0 v

endpointIntersectionResolved ::
  (Intersection.Kind, Sign) ->
  Domain ->
  Domain ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Fuzzy Bool
endpointIntersectionResolved intersectionType _ _ segment1 segment2 =
  case computeIntersectionType segment1 segment2 of
    Resolved localIntersectionType -> Resolved (localIntersectionType == intersectionType)
    Unresolved -> Unresolved

computeIntersectionType ::
  Segment (space @ units) ->
  Segment (space @ units) ->
  Fuzzy (Intersection.Kind, Sign)
computeIntersectionType segment1 segment2 =
  if Qty.abs firstResolution >= 0.5
    then Resolved (Intersection.Crossing, Qty.sign firstResolution)
    else
      let dX1_dU1 = VectorBox2d.xComponent firstBounds1
          dY1_dU1 = VectorBox2d.yComponent firstBounds1
          dX2_dU2 = VectorBox2d.xComponent firstBounds2
          dY2_dU2 = VectorBox2d.yComponent firstBounds2
          d2X1_dU1dU1 = VectorBox2d.xComponent secondBounds1
          d2Y1_dU1dU1 = VectorBox2d.yComponent secondBounds1
          d2X2_dU2dU2 = VectorBox2d.xComponent secondBounds2
          d2Y2_dU2dU2 = VectorBox2d.yComponent secondBounds2
          resolutionXY =
            secondResolution1d
              dX1_dU1
              dY1_dU1
              dX2_dU2
              dY2_dU2
              d2X1_dU1dU1
              d2Y1_dU1dU1
              d2X2_dU2dU2
              d2Y2_dU2dU2
          resolutionYX =
            secondResolution1d
              dY1_dU1
              dX1_dU1
              dY2_dU2
              dX2_dU2
              d2Y1_dU1dU1
              d2X1_dU1dU1
              d2Y2_dU2dU2
              d2X2_dU2dU2
          secondResolution =
            if Qty.abs resolutionXY >= Qty.abs resolutionYX
              then resolutionXY
              else -resolutionYX
       in if Qty.abs secondResolution >= 0.5
            then Resolved (Intersection.Tangent, Qty.sign secondResolution)
            else Unresolved
 where
  (Segment _ firstBounds1 secondBounds1 _) = segment1
  (Segment _ firstBounds2 secondBounds2 _) = segment2
  firstResolution = crossProductResolution segment1 segment2

isTangentIntersectionCandidate ::
  (Tolerance units) =>
  Domain ->
  Domain ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Bool
isTangentIntersectionCandidate _ _ segment1 segment2 =
  overlaps segment1 segment2 && Qty.abs (crossProductResolution segment1 segment2) < 0.1

tangentIntersectionSign ::
  Domain ->
  Domain ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Fuzzy Sign
tangentIntersectionSign _ _ segment1 segment2 =
  let (Segment _ firstBounds1 secondBounds1 _) = segment1
      (Segment _ firstBounds2 secondBounds2 _) = segment2
      dX1_dU1 = VectorBox2d.xComponent firstBounds1
      dY1_dU1 = VectorBox2d.yComponent firstBounds1
      dX2_dU2 = VectorBox2d.xComponent firstBounds2
      dY2_dU2 = VectorBox2d.yComponent firstBounds2
      d2X1_dU1dU1 = VectorBox2d.xComponent secondBounds1
      d2Y1_dU1dU1 = VectorBox2d.yComponent secondBounds1
      d2X2_dU2dU2 = VectorBox2d.xComponent secondBounds2
      d2Y2_dU2dU2 = VectorBox2d.yComponent secondBounds2
      resolutionXY =
        secondResolution1d
          dX1_dU1
          dY1_dU1
          dX2_dU2
          dY2_dU2
          d2X1_dU1dU1
          d2Y1_dU1dU1
          d2X2_dU2dU2
          d2Y2_dU2dU2
      resolutionYX =
        secondResolution1d
          dY1_dU1
          dX1_dU1
          dY2_dU2
          dX2_dU2
          d2Y1_dU1dU1
          d2X1_dU1dU1
          d2Y2_dU2dU2
          d2X2_dU2dU2
      secondResolution =
        if Qty.abs resolutionXY >= Qty.abs resolutionYX
          then resolutionXY
          else -resolutionYX
   in if Qty.abs secondResolution >= 0.5
        then Resolved (Qty.sign secondResolution)
        else Unresolved

secondResolution1d ::
  Range units ->
  Range units ->
  Range units ->
  Range units ->
  Range units ->
  Range units ->
  Range units ->
  Range units ->
  Float
secondResolution1d dX1_dU1 dY1_dU1 dX2_dU2 dY2_dU2 d2X1_dU1dU1 d2Y1_dU1dU1 d2X2_dU2dU2 d2Y2_dU2dU2
  | Range.includes Qty.zero dX1_dU1 || Range.includes Qty.zero dX2_dU2 = 0.0
  | otherwise =
      let d2Y1_dXdX = secondDerivativeBounds1d dX1_dU1 dY1_dU1 d2X1_dU1dU1 d2Y1_dU1dU1
          d2Y2_dXdX = secondDerivativeBounds1d dX2_dU2 dY2_dU2 d2X2_dU2dU2 d2Y2_dU2dU2
       in Range.resolution (d2Y2_dXdX - d2Y1_dXdX)

secondDerivativeBounds1d ::
  Range units ->
  Range units ->
  Range units ->
  Range units ->
  Range (Unitless :/ units)
secondDerivativeBounds1d dXdU dYdU d2XdU2 d2YdU2 =
  let dXdU' = Units.generalize dXdU
      dYdU' = Units.generalize dYdU
      d2XdU2' = Units.generalize d2XdU2
      d2YdU2' = Units.generalize d2YdU2
   in Units.generalize ((d2YdU2' * dXdU' - dYdU' * d2XdU2') / (dXdU' * dXdU')) ./ dXdU'

findTangentIntersection ::
  (Tolerance units) =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  Domain ->
  Domain ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Sign ->
  Maybe Intersection
findTangentIntersection derivatives1 derivatives2 u v _ _ sign = do
  (u0, v0) <- Range.find2 (isTangentIntersection derivatives1 derivatives2) u v
  Just (Intersection u0 v0 Intersection.Tangent sign)

isTangentIntersection ::
  (Tolerance units) =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  Domain ->
  Domain ->
  Bool
isTangentIntersection derivatives1 derivatives2 u1 u2 =
  let bounds1 = Curve2d.segmentBounds u1 derivatives1.curve
      bounds2 = Curve2d.segmentBounds u2 derivatives2.curve
      difference = bounds1 - bounds2
      distance = VectorBox2d.magnitude difference
   in Range.minValue distance <= ?tolerance
        && let firstBounds1 = VectorCurve2d.segmentBounds u1 derivatives1.first
               firstBounds2 = VectorCurve2d.segmentBounds u2 derivatives2.first
               crossProduct = Units.generalize firstBounds1 >< Units.generalize firstBounds2
               dotProduct1 = Units.generalize firstBounds1 <> Units.generalize difference
               dotProduct2 = Units.generalize firstBounds2 <> Units.generalize difference
            in Range.includes Qty.zero crossProduct
                && Range.includes Qty.zero dotProduct1
                && Range.includes Qty.zero dotProduct2

isCrossingIntersectionCandidate ::
  (Tolerance units) =>
  Domain ->
  Domain ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Bool
isCrossingIntersectionCandidate _ _ segment1 segment2 = overlaps segment1 segment2

crossingIntersectionSign ::
  Domain ->
  Domain ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Fuzzy Sign
crossingIntersectionSign _ _ segment1 segment2
  | Qty.abs firstResolution >= 0.5 = Resolved (Qty.sign firstResolution)
  | otherwise = Unresolved
 where
  firstResolution = crossProductResolution segment1 segment2

findCrossingIntersection ::
  (Tolerance units) =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  Domain ->
  Domain ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Sign ->
  Maybe Intersection
findCrossingIntersection derivatives1 derivatives2 u v _ _ sign = do
  (u0, v0) <- Range.find2 (isCrossingIntersection derivatives1.curve derivatives2.curve) u v
  Just (Intersection u0 v0 Intersection.Crossing sign)

isCrossingIntersection ::
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Domain ->
  Domain ->
  Bool
isCrossingIntersection curve1 curve2 u v =
  BoundingBox2d.intersects
    (Curve2d.segmentBounds u curve1)
    (Curve2d.segmentBounds v curve2)
