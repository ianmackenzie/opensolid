module Curve2d.Segment
  ( Segment
  , init
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

import Bounds2d (Bounds2d)
import {-# SOURCE #-} Curve2d (Curve2d)
import {-# SOURCE #-} Curve2d qualified
import Curve2d.Derivatives (Derivatives)
import Curve2d.Derivatives qualified as Derivatives
import Curve2d.Intersection (Intersection)
import Curve2d.Intersection qualified as Intersection
import DirectionBounds2d (DirectionBounds2d)
import DirectionCurve2d qualified
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified
import U qualified
import Units ((:/))
import Units qualified
import VectorBounds2d (VectorBounds2d)
import VectorBounds2d qualified
import VectorCurve2d qualified

data Segment (coordinateSystem :: CoordinateSystem) where
  Segment ::
    { curveBounds :: Bounds2d (space @ units)
    , firstBounds :: ~(VectorBounds2d (space @ units))
    , secondBounds :: ~(VectorBounds2d (space @ units))
    , tangentBounds :: ~(DirectionBounds2d space)
    } ->
    Segment (space @ units)

init ::
  (Tolerance units) =>
  Derivatives (space @ units) ->
  U.Bounds ->
  Segment (space @ units)
init derivatives domain =
  Segment
    { curveBounds = Curve2d.segmentBounds domain (Derivatives.curve derivatives)
    , firstBounds = VectorCurve2d.segmentBounds domain (Derivatives.first derivatives)
    , secondBounds = VectorCurve2d.segmentBounds domain (Derivatives.second derivatives)
    , tangentBounds = DirectionCurve2d.segmentBounds domain (Curve2d.tangentDirection (Derivatives.curve derivatives))
    }

crossProductResolution :: Segment (space @ units) -> Segment (space @ units) -> Float
crossProductResolution segment1 segment2 =
  Range.resolution (tangentBounds segment1 >< tangentBounds segment2)

isEndpointIntersectionCandidate ::
  (Float, Float) ->
  U.Bounds ->
  U.Bounds ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Bool
isEndpointIntersectionCandidate (u0, v0) u v _ _ =
  Range.includes u0 u && Range.includes v0 v

endpointIntersectionResolved ::
  (Intersection.Kind, Sign) ->
  U.Bounds ->
  U.Bounds ->
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
  let firstResolution = crossProductResolution segment1 segment2
   in if Qty.abs firstResolution >= 0.5
        then Resolved (Intersection.Crossing, Qty.sign firstResolution)
        else
          let firstBounds1 = firstBounds segment1
              firstBounds2 = firstBounds segment2
              dX1_dU1 = VectorBounds2d.xComponent firstBounds1
              dY1_dU1 = VectorBounds2d.yComponent firstBounds1
              dX2_dU2 = VectorBounds2d.xComponent firstBounds2
              dY2_dU2 = VectorBounds2d.yComponent firstBounds2
              secondBounds1 = secondBounds segment1
              secondBounds2 = secondBounds segment2
              d2X1_dU1dU1 = VectorBounds2d.xComponent secondBounds1
              d2Y1_dU1dU1 = VectorBounds2d.yComponent secondBounds1
              d2X2_dU2dU2 = VectorBounds2d.xComponent secondBounds2
              d2Y2_dU2dU2 = VectorBounds2d.yComponent secondBounds2
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

isTangentIntersectionCandidate ::
  (Tolerance units) =>
  U.Bounds ->
  U.Bounds ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Bool
isTangentIntersectionCandidate _ _ segment1 segment2 =
  curveBounds segment1 ^ curveBounds segment2
    && Qty.abs (crossProductResolution segment1 segment2) < 0.1

tangentIntersectionSign ::
  U.Bounds ->
  U.Bounds ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Fuzzy Sign
tangentIntersectionSign _ _ segment1 segment2 =
  let firstBounds1 = firstBounds segment1
      firstBounds2 = firstBounds segment2
      dX1_dU1 = VectorBounds2d.xComponent firstBounds1
      dY1_dU1 = VectorBounds2d.yComponent firstBounds1
      dX2_dU2 = VectorBounds2d.xComponent firstBounds2
      dY2_dU2 = VectorBounds2d.yComponent firstBounds2
      secondBounds1 = secondBounds segment1
      secondBounds2 = secondBounds segment2
      d2X1_dU1dU1 = VectorBounds2d.xComponent secondBounds1
      d2Y1_dU1dU1 = VectorBounds2d.yComponent secondBounds1
      d2X2_dU2dU2 = VectorBounds2d.xComponent secondBounds2
      d2Y2_dU2dU2 = VectorBounds2d.yComponent secondBounds2
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
  U.Bounds ->
  U.Bounds ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Sign ->
  Maybe Intersection
findTangentIntersection derivatives1 derivatives2 uBounds1 uBounds2 _ _ sign = do
  (u1, u2) <- Range.find2 (isTangentIntersection derivatives1 derivatives2) uBounds1 uBounds2
  Just (Intersection.tangent u1 u2 sign)

isTangentIntersection ::
  (Tolerance units) =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  U.Bounds ->
  U.Bounds ->
  Bool
isTangentIntersection derivatives1 derivatives2 u1 u2 =
  let bounds1 = Curve2d.segmentBounds u1 (Derivatives.curve derivatives1)
      bounds2 = Curve2d.segmentBounds u2 (Derivatives.curve derivatives2)
      difference = bounds1 - bounds2
      distance = VectorBounds2d.magnitude difference
   in Range.minValue distance <= ?tolerance
        && let firstBounds1 = VectorCurve2d.segmentBounds u1 (Derivatives.first derivatives1)
               firstBounds2 = VectorCurve2d.segmentBounds u2 (Derivatives.first derivatives2)
               crossProduct = Units.generalize firstBounds1 >< Units.generalize firstBounds2
               dotProduct1 = Units.generalize firstBounds1 <> Units.generalize difference
               dotProduct2 = Units.generalize firstBounds2 <> Units.generalize difference
            in Range.includes Qty.zero crossProduct
                && Range.includes Qty.zero dotProduct1
                && Range.includes Qty.zero dotProduct2

isCrossingIntersectionCandidate ::
  (Tolerance units) =>
  U.Bounds ->
  U.Bounds ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Bool
isCrossingIntersectionCandidate _ _ segment1 segment2 =
  curveBounds segment1 ^ curveBounds segment2

crossingIntersectionSign ::
  U.Bounds ->
  U.Bounds ->
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
  U.Bounds ->
  U.Bounds ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Sign ->
  Maybe Intersection
findCrossingIntersection derivatives1 derivatives2 uBounds1 uBounds2 _ _ sign = do
  let curve1 = Derivatives.curve derivatives1
  let curve2 = Derivatives.curve derivatives2
  (u1, u2) <- Range.find2 (isCrossingIntersection curve1 curve2) uBounds1 uBounds2
  Just (Intersection.crossing u1 u2 sign)

isCrossingIntersection ::
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  U.Bounds ->
  U.Bounds ->
  Bool
isCrossingIntersection curve1 curve2 u1 u2 =
  exactly (Curve2d.segmentBounds u1 curve1 ^ Curve2d.segmentBounds u2 curve2)
