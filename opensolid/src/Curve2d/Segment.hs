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
import Bounds2d qualified
import {-# SOURCE #-} Curve2d (Curve2d)
import {-# SOURCE #-} Curve2d qualified
import Curve2d.Derivatives (Derivatives)
import Curve2d.Derivatives qualified as Derivatives
import Curve2d.IntersectionPoint (IntersectionPoint)
import Curve2d.IntersectionPoint qualified as IntersectionPoint
import Maybe qualified
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified
import VectorBounds2d (VectorBounds2d)
import VectorBounds2d qualified
import VectorCurve2d qualified

data Segment (coordinateSystem :: CoordinateSystem) where
  Segment ::
    { curveBounds :: Bounds2d (space @ units)
    , firstBounds :: ~(VectorBounds2d (space @ units))
    , secondBounds :: ~(VectorBounds2d (space @ units))
    } ->
    Segment (space @ units)

init ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Range Unitless ->
  Segment (space @ units)
init derivatives domain =
  Segment
    { curveBounds = Curve2d.evaluateBounds (Derivatives.curve derivatives) domain
    , firstBounds = VectorCurve2d.evaluateBounds (Derivatives.first derivatives) domain
    , secondBounds = VectorCurve2d.evaluateBounds (Derivatives.second derivatives) domain
    }

crossProductResolution :: Segment (space @ units) -> Segment (space @ units) -> Float
crossProductResolution segment1 segment2 =
  Range.resolution (firstBounds segment1 .><. firstBounds segment2)

isEndpointIntersectionCandidate ::
  (Float, Float) ->
  Range Unitless ->
  Range Unitless ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Bool
isEndpointIntersectionCandidate (tValue1, tValue2) tRange1 tRange2 _ _ =
  Range.includes tValue1 tRange1 && Range.includes tValue2 tRange2

endpointIntersectionResolved ::
  (IntersectionPoint.Kind, Sign) ->
  Range Unitless ->
  Range Unitless ->
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
  Fuzzy (IntersectionPoint.Kind, Sign)
computeIntersectionType segment1 segment2 = do
  let firstResolution = crossProductResolution segment1 segment2
  if Qty.abs firstResolution >= 0.5
    then Resolved (IntersectionPoint.Crossing, Qty.sign firstResolution)
    else do
      let firstBounds1 = firstBounds segment1
      let firstBounds2 = firstBounds segment2
      let dX1_dU1 = VectorBounds2d.xComponent firstBounds1
      let dY1_dU1 = VectorBounds2d.yComponent firstBounds1
      let dX2_dU2 = VectorBounds2d.xComponent firstBounds2
      let dY2_dU2 = VectorBounds2d.yComponent firstBounds2
      let secondBounds1 = secondBounds segment1
      let secondBounds2 = secondBounds segment2
      let d2X1_dU1dU1 = VectorBounds2d.xComponent secondBounds1
      let d2Y1_dU1dU1 = VectorBounds2d.yComponent secondBounds1
      let d2X2_dU2dU2 = VectorBounds2d.xComponent secondBounds2
      let d2Y2_dU2dU2 = VectorBounds2d.yComponent secondBounds2
      let resolutionXY =
            secondResolution1d
              dX1_dU1
              dY1_dU1
              dX2_dU2
              dY2_dU2
              d2X1_dU1dU1
              d2Y1_dU1dU1
              d2X2_dU2dU2
              d2Y2_dU2dU2
      let resolutionYX =
            secondResolution1d
              dY1_dU1
              dX1_dU1
              dY2_dU2
              dX2_dU2
              d2Y1_dU1dU1
              d2X1_dU1dU1
              d2Y2_dU2dU2
              d2X2_dU2dU2
      let secondResolution =
            if Qty.abs resolutionXY >= Qty.abs resolutionYX
              then resolutionXY
              else -resolutionYX
      if Qty.abs secondResolution >= 0.5
        then Resolved (IntersectionPoint.Tangent, Qty.sign secondResolution)
        else Unresolved

isTangentIntersectionCandidate ::
  Tolerance units =>
  Range Unitless ->
  Range Unitless ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Bool
isTangentIntersectionCandidate _ _ segment1 segment2 =
  curveBounds segment1 ^ curveBounds segment2
    && Qty.abs (crossProductResolution segment1 segment2) < 0.1

tangentIntersectionSign ::
  Range Unitless ->
  Range Unitless ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Fuzzy Sign
tangentIntersectionSign _ _ segment1 segment2 = do
  let firstBounds1 = firstBounds segment1
  let firstBounds2 = firstBounds segment2
  let dX1_dU1 = VectorBounds2d.xComponent firstBounds1
  let dY1_dU1 = VectorBounds2d.yComponent firstBounds1
  let dX2_dU2 = VectorBounds2d.xComponent firstBounds2
  let dY2_dU2 = VectorBounds2d.yComponent firstBounds2
  let secondBounds1 = secondBounds segment1
  let secondBounds2 = secondBounds segment2
  let d2X1_dU1dU1 = VectorBounds2d.xComponent secondBounds1
  let d2Y1_dU1dU1 = VectorBounds2d.yComponent secondBounds1
  let d2X2_dU2dU2 = VectorBounds2d.xComponent secondBounds2
  let d2Y2_dU2dU2 = VectorBounds2d.yComponent secondBounds2
  let resolutionXY =
        secondResolution1d
          dX1_dU1
          dY1_dU1
          dX2_dU2
          dY2_dU2
          d2X1_dU1dU1
          d2Y1_dU1dU1
          d2X2_dU2dU2
          d2Y2_dU2dU2
  let resolutionYX =
        secondResolution1d
          dY1_dU1
          dX1_dU1
          dY2_dU2
          dX2_dU2
          d2Y1_dU1dU1
          d2X1_dU1dU1
          d2Y2_dU2dU2
          d2X2_dU2dU2
  let secondResolution =
        if Qty.abs resolutionXY >= Qty.abs resolutionYX
          then resolutionXY
          else -resolutionYX
  if Qty.abs secondResolution >= 0.5
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
  | otherwise = do
      let d2Y1_dXdX = secondDerivativeBounds1d dX1_dU1 dY1_dU1 d2X1_dU1dU1 d2Y1_dU1dU1
      let d2Y2_dXdX = secondDerivativeBounds1d dX2_dU2 dY2_dU2 d2X2_dU2dU2 d2Y2_dU2dU2
      Range.resolution (d2Y2_dXdX - d2Y1_dXdX)

secondDerivativeBounds1d ::
  Range units ->
  Range units ->
  Range units ->
  Range units ->
  Range (Unitless :/: units)
secondDerivativeBounds1d dXdU dYdU d2XdU2 d2YdU2 =
  (d2YdU2 .*. dXdU - dYdU .*. d2XdU2) !?/.!? (dXdU .*. dXdU .*. dXdU)

findTangentIntersection ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  Range Unitless ->
  Range Unitless ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Sign ->
  Maybe IntersectionPoint
findTangentIntersection derivatives1 derivatives2 tRange1 tRange2 _ _ sign = Maybe.do
  (t1, t2) <- Range.find2 (isTangentIntersection derivatives1 derivatives2) tRange1 tRange2
  Just (IntersectionPoint.tangent t1 t2 sign)

isTangentIntersection ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  Range Unitless ->
  Range Unitless ->
  Bool
isTangentIntersection derivatives1 derivatives2 tRange1 tRange2 = do
  let bounds1 = Curve2d.evaluateBounds (Derivatives.curve derivatives1) tRange1
  let bounds2 = Curve2d.evaluateBounds (Derivatives.curve derivatives2) tRange2
  let difference = bounds1 - bounds2
  let distance = VectorBounds2d.magnitude difference
  let firstBounds1 = VectorCurve2d.evaluateBounds (Derivatives.first derivatives1) tRange1
  let firstBounds2 = VectorCurve2d.evaluateBounds (Derivatives.first derivatives2) tRange2
  let crossProduct = firstBounds1 .><. firstBounds2
  let dotProduct1 = firstBounds1 .<>. difference
  let dotProduct2 = firstBounds2 .<>. difference
  Range.minValue distance <= ?tolerance
    && Range.includes Qty.zero crossProduct
    && Range.includes Qty.zero dotProduct1
    && Range.includes Qty.zero dotProduct2

isCrossingIntersectionCandidate ::
  Tolerance units =>
  Range Unitless ->
  Range Unitless ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Bool
isCrossingIntersectionCandidate _ _ segment1 segment2 =
  curveBounds segment1 ^ curveBounds segment2

crossingIntersectionSign ::
  Range Unitless ->
  Range Unitless ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Fuzzy Sign
crossingIntersectionSign _ _ segment1 segment2 = do
  let firstResolution = crossProductResolution segment1 segment2
  if Qty.abs firstResolution >= 0.5
    then Resolved (Qty.sign firstResolution)
    else Unresolved

findCrossingIntersection ::
  Tolerance units =>
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  Range Unitless ->
  Range Unitless ->
  Segment (space @ units) ->
  Segment (space @ units) ->
  Sign ->
  Maybe IntersectionPoint
findCrossingIntersection derivatives1 derivatives2 tRange1 tRange2 _ _ sign = Maybe.do
  let curve1 = Derivatives.curve derivatives1
  let curve2 = Derivatives.curve derivatives2
  (t1, t2) <- Range.find2 (isCrossingIntersection curve1 curve2) tRange1 tRange2
  Just (IntersectionPoint.crossing t1 t2 sign)

isCrossingIntersection ::
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Range Unitless ->
  Range Unitless ->
  Bool
isCrossingIntersection curve1 curve2 tRange1 tRange2 = do
  let curveBounds1 = Curve2d.evaluateBounds curve1 tRange1
  let curveBounds2 = Curve2d.evaluateBounds curve2 tRange2
  Bounds2d.overlap curveBounds1 curveBounds2 >= Qty.zero
