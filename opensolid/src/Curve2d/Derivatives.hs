module Curve2d.Derivatives
  ( Derivatives (curve, first, second)
  , ofCurve
  , classify
  )
where

import {-# SOURCE #-} Curve2d (Curve2d)
import {-# SOURCE #-} Curve2d qualified
import Curve2d.Intersection (TangentIntersectionAtDegeneratePoint (TangentIntersectionAtDegeneratePoint))
import Curve2d.Intersection qualified as Intersection
import OpenSolid
import Qty qualified
import Units qualified
import Vector2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

data Derivatives (coordinateSystem :: CoordinateSystem) = Derivatives
  { curve :: Curve2d coordinateSystem
  , first :: VectorCurve2d coordinateSystem
  , second :: VectorCurve2d coordinateSystem
  }

ofCurve :: Tolerance units => Curve2d (space @ units) -> Derivatives (space @ units)
ofCurve givenCurve = do
  let firstDerivative = Curve2d.derivative givenCurve
  let secondDerivative = VectorCurve2d.derivative firstDerivative
  Derivatives
    { curve = givenCurve
    , first = firstDerivative
    , second = secondDerivative
    }

classify ::
  Tolerance units =>
  (Float, Float) ->
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  Result TangentIntersectionAtDegeneratePoint (Intersection.Kind, Sign)
classify (u, v) derivatives1 derivatives2 = do
  let first1 = VectorCurve2d.evaluateAt u (first derivatives1)
  let first2 = VectorCurve2d.evaluateAt v (first derivatives2)
  let second1 = VectorCurve2d.evaluateAt u (second derivatives1)
  let second2 = VectorCurve2d.evaluateAt v (second derivatives2)
  let first1Magnitude = Vector2d.magnitude first1
  let first2Magnitude = Vector2d.magnitude first2
  let second1Magnitude = Vector2d.magnitude second1
  let second2Magnitude = Vector2d.magnitude second2
  let first1Zero = first1Magnitude < ?tolerance
  let first2Zero = first2Magnitude < ?tolerance
  let tangent1 = if first1Zero then second1 / second1Magnitude else first1 / first2Magnitude
  let tangent2 = if first2Zero then second2 / second2Magnitude else first2 / first2Magnitude
  let tangentCrossProduct = tangent1 >< tangent2
  let crossProductMagnitude = Qty.abs tangentCrossProduct
  let sign0 = Qty.sign tangentCrossProduct
  let radius0 = ?tolerance / crossProductMagnitude
  if first1Zero || first2Zero
    then do
      let length1 = if first1Zero then 0.5 * second1Magnitude else first1Magnitude
      let length2 = if first2Zero then 0.5 * second2Magnitude else first2Magnitude
      if crossProductMagnitude * Qty.min length1 length2 < ?tolerance
        then Error TangentIntersectionAtDegeneratePoint
        else Ok (Intersection.Crossing, sign0)
    else
      if crossProductMagnitude > 0.1
        then Ok (Intersection.Crossing, sign0)
        else do
          let dX1_dU1 = first1Magnitude
          let dY1_dU1 = Qty.zero
          let dX2_dU2 = tangent1 <> first2
          let dY2_dU2 = tangent1 >< first2
          let d2X1_dU1dU1 = tangent1 <> second1
          let d2Y1_dU1dU1 = tangent1 >< second1
          let d2X2_dU2dU2 = tangent1 <> second1
          let d2Y2_dU2dU2 = tangent1 >< second1
          let d2Y1_dXdX = secondDerivative1d dX1_dU1 dY1_dU1 d2X1_dU1dU1 d2Y1_dU1dU1
          let d2Y2_dXdX = secondDerivative1d dX2_dU2 dY2_dU2 d2X2_dU2dU2 d2Y2_dU2dU2
          let d2Y_dXdX = d2Y2_dXdX - d2Y1_dXdX
          let sign1 = Qty.sign d2Y_dXdX
          let radius1 = Qty.sqrt_ (2.0 * ?tolerance ./^ Qty.abs d2Y_dXdX)
          if radius0 <= radius1
            then Ok (Intersection.Crossing, sign0)
            else Ok (Intersection.Tangent, sign1)

secondDerivative1d :: Qty units -> Qty units -> Qty units -> Qty units -> Qty (Units.GenericQuotient Unitless units)
secondDerivative1d dXdU dYdU d2XdU2 d2YdU2 =
  (d2YdU2 .*. dXdU - dYdU .*. d2XdU2) !?/.!? (dXdU .*. dXdU .*. dXdU)
