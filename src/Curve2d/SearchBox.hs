module Curve2d.SearchBox
  ( SearchBox
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

import BoundingBox2d qualified
import CoordinateSystem (Space)
import {-# SOURCE #-} Curve2d (Curve2d)
import {-# SOURCE #-} Curve2d qualified
import Curve2d.Derivatives (Derivatives)
import Curve2d.Intersection (Intersection (Intersection))
import Curve2d.Intersection qualified as Intersection
import Domain (Domain)
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified
import Units (Unitless, (:/))
import Units qualified
import VectorBox2d (VectorBox2d)
import VectorBox2d qualified
import VectorCurve2d qualified

data SearchBox (coordinateSystem :: CoordinateSystem)
  = SearchBox
      Bool
      ~(VectorBox2d coordinateSystem)
      ~(VectorBox2d coordinateSystem)
      ~(VectorBox2d coordinateSystem)
      ~(VectorBox2d coordinateSystem)
      ~(VectorBox2d (Space coordinateSystem @ Unitless))
      ~(VectorBox2d (Space coordinateSystem @ Unitless))
      Float

init
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Domain
  -> Domain
  -> SearchBox (space @ units)
init derivatives1 derivatives2 u v =
  let curveBounds1 = Curve2d.segmentBounds u derivatives1.curve
      curveBounds2 = Curve2d.segmentBounds v derivatives2.curve
      difference = curveBounds1 - curveBounds2
      isCandidate = Range.minValue (VectorBox2d.magnitude difference) <= ?tolerance
      firstBounds1 = VectorCurve2d.segmentBounds u derivatives1.first
      firstBounds2 = VectorCurve2d.segmentBounds v derivatives2.first
      secondBounds1 = VectorCurve2d.segmentBounds u derivatives1.second
      secondBounds2 = VectorCurve2d.segmentBounds v derivatives2.second
      tangentBounds1 = computeTangentBounds derivatives1 u firstBounds1 secondBounds1
      tangentBounds2 = computeTangentBounds derivatives2 u firstBounds2 secondBounds2
      tangentCrossProduct = tangentBounds1 >< tangentBounds2
      firstResolution = Range.resolution tangentCrossProduct
   in SearchBox
        isCandidate
        firstBounds1
        firstBounds2
        secondBounds1
        secondBounds2
        tangentBounds1
        tangentBounds2
        firstResolution

computeTangentBounds
  :: Derivatives (space @ units)
  -> Domain
  -> VectorBox2d (space @ units)
  -> VectorBox2d (space @ units)
  -> VectorBox2d (space @ Unitless)
computeTangentBounds derivatives u firstBounds secondBounds
  | Range.includes 0.0 u && derivatives.degenerateStart = VectorBox2d.normalize secondBounds
  | Range.includes 1.0 u && derivatives.degenerateEnd = -(VectorBox2d.normalize secondBounds)
  | otherwise = VectorBox2d.normalize firstBounds

isEndpointIntersectionCandidate
  :: (Float, Float)
  -> Domain
  -> Domain
  -> SearchBox (space @ units)
  -> Bool
isEndpointIntersectionCandidate (u0, v0) u v _ =
  Range.includes u0 u && Range.includes v0 v

endpointIntersectionResolved
  :: (Intersection.Kind, Sign)
  -> Domain
  -> Domain
  -> SearchBox (space @ units)
  -> Fuzzy Bool
endpointIntersectionResolved intersectionType _ _ searchBox =
  case computeIntersectionType searchBox of
    Resolved localIntersectionType -> Resolved (localIntersectionType == intersectionType)
    Unresolved -> Unresolved

computeIntersectionType :: SearchBox (space @ units) -> Fuzzy (Intersection.Kind, Sign)
computeIntersectionType searchBox =
  if Qty.abs firstResolution >= 0.5
    then Resolved (Intersection.Crossing, Qty.sign firstResolution)
    else
      let dX1_dU1 = firstBounds1.xComponent
          dY1_dU1 = firstBounds1.yComponent
          dX2_dU2 = firstBounds2.xComponent
          dY2_dU2 = firstBounds2.yComponent
          d2X1_dU1dU1 = secondBounds1.xComponent
          d2Y1_dU1dU1 = secondBounds1.yComponent
          d2X2_dU2dU2 = secondBounds2.xComponent
          d2Y2_dU2dU2 = secondBounds2.yComponent
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
  (SearchBox _ firstBounds1 firstBounds2 secondBounds1 secondBounds2 _ _ firstResolution) = searchBox

isTangentIntersectionCandidate :: Domain -> Domain -> SearchBox (space @ units) -> Bool
isTangentIntersectionCandidate _ _ (SearchBox isCandidate _ _ _ _ _ _ firstResolution) =
  isCandidate && Qty.abs firstResolution < 0.5

tangentIntersectionSign :: Domain -> Domain -> SearchBox (space @ units) -> Fuzzy Sign
tangentIntersectionSign _ _ (SearchBox _ firstBounds1 firstBounds2 secondBounds1 secondBounds2 _ _ _) =
  let dX1_dU1 = firstBounds1.xComponent
      dY1_dU1 = firstBounds1.yComponent
      dX2_dU2 = firstBounds2.xComponent
      dY2_dU2 = firstBounds2.yComponent
      d2X1_dU1dU1 = secondBounds1.xComponent
      d2Y1_dU1dU1 = secondBounds1.yComponent
      d2X2_dU2dU2 = secondBounds2.xComponent
      d2Y2_dU2dU2 = secondBounds2.yComponent
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

secondResolution1d
  :: Range units
  -> Range units
  -> Range units
  -> Range units
  -> Range units
  -> Range units
  -> Range units
  -> Range units
  -> Float
secondResolution1d dX1_dU1 dY1_dU1 dX2_dU2 dY2_dU2 d2X1_dU1dU1 d2Y1_dU1dU1 d2X2_dU2dU2 d2Y2_dU2dU2
  | Range.includes Qty.zero dX1_dU1 || Range.includes Qty.zero dX2_dU2 = 0.0
  | otherwise =
      let d2Y1_dXdX = secondDerivativeBounds1d dX1_dU1 dY1_dU1 d2X1_dU1dU1 d2Y1_dU1dU1
          d2Y2_dXdX = secondDerivativeBounds1d dX2_dU2 dY2_dU2 d2X2_dU2dU2 d2Y2_dU2dU2
       in Range.resolution (d2Y2_dXdX - d2Y1_dXdX)

secondDerivativeBounds1d
  :: Range units
  -> Range units
  -> Range units
  -> Range units
  -> Range (Unitless :/ units)
secondDerivativeBounds1d dXdU dYdU d2XdU2 d2YdU2 =
  let dXdU' = Units.generalize dXdU
      dYdU' = Units.generalize dYdU
      d2XdU2' = Units.generalize d2XdU2
      d2YdU2' = Units.generalize d2YdU2
   in Units.generalize ((d2YdU2' * dXdU' - dYdU' * d2XdU2') / (dXdU' * dXdU')) ./ dXdU'

findTangentIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Domain
  -> Domain
  -> SearchBox (sapce @ units)
  -> Sign
  -> Maybe Intersection
findTangentIntersection derivatives1 derivatives2 u v _ sign = do
  (u0, v0) <- Range.find2 (isTangentIntersection derivatives1 derivatives2) u v
  Just (Intersection u0 v0 Intersection.Tangent sign)

isTangentIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Domain
  -> Domain
  -> Bool
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

isCrossingIntersectionCandidate :: Domain -> Domain -> SearchBox (space @ units) -> Bool
isCrossingIntersectionCandidate _ _ (SearchBox isCandidate _ _ _ _ _ _ _) = isCandidate

crossingIntersectionSign :: Domain -> Domain -> SearchBox (space @ units) -> Fuzzy Sign
crossingIntersectionSign _ _ (SearchBox _ _ _ _ _ _ _ firstResolution)
  | Qty.abs firstResolution >= 0.5 = Resolved (Qty.sign firstResolution)
  | otherwise = Unresolved

findCrossingIntersection
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Domain
  -> Domain
  -> SearchBox (space @ units)
  -> Sign
  -> Maybe Intersection
findCrossingIntersection derivatives1 derivatives2 u v _ sign = do
  (u0, v0) <- Range.find2 (isCrossingIntersection derivatives1.curve derivatives2.curve) u v
  Just (Intersection u0 v0 Intersection.Crossing sign)

isCrossingIntersection
  :: Curve2d (space @ units)
  -> Curve2d (space @ units)
  -> Domain
  -> Domain
  -> Bool
isCrossingIntersection curve1 curve2 u v =
  BoundingBox2d.intersects
    (Curve2d.segmentBounds u curve1)
    (Curve2d.segmentBounds v curve2)
