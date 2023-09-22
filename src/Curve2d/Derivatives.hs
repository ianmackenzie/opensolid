module Curve2d.Derivatives
  ( Derivatives (..)
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
import Units ((:/))
import Units qualified
import Vector2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

data Derivatives (coordinateSystem :: CoordinateSystem)
  = Derivatives
      (Curve2d coordinateSystem)
      (VectorCurve2d coordinateSystem)
      (VectorCurve2d coordinateSystem)
      Bool
      Bool

instance HasField "curve" (Derivatives (space @ units)) (Curve2d (space @ units)) where
  getField (Derivatives field _ _ _ _) = field

instance HasField "first" (Derivatives (space @ units)) (VectorCurve2d (space @ units)) where
  getField (Derivatives _ field _ _ _) = field

instance HasField "second" (Derivatives (space @ units)) (VectorCurve2d (space @ units)) where
  getField (Derivatives _ _ field _ _) = field

instance HasField "degenerateStart" (Derivatives (space @ units)) Bool where
  getField (Derivatives _ _ _ field _) = field

instance HasField "degenerateEnd" (Derivatives (space @ units)) Bool where
  getField (Derivatives _ _ _ _ field) = field

ofCurve :: (Tolerance units) => Curve2d (space @ units) -> Derivatives (space @ units)
ofCurve givenCurve =
  let firstDerivative = Curve2d.derivative givenCurve
      secondDerivative = VectorCurve2d.derivative firstDerivative
   in Derivatives
        givenCurve
        firstDerivative
        secondDerivative
        (isDegenerateAt 0.0 firstDerivative)
        (isDegenerateAt 1.0 firstDerivative)

isDegenerateAt :: (Tolerance units) => Float -> VectorCurve2d (space @ units) -> Bool
isDegenerateAt u firstDerivative =
  Vector2d.magnitude (VectorCurve2d.evaluateAt u firstDerivative) ~= Qty.zero

classify ::
  (Tolerance units) =>
  (Float, Float) ->
  Derivatives (space @ units) ->
  Derivatives (space @ units) ->
  Result TangentIntersectionAtDegeneratePoint (Intersection.Kind, Sign)
classify (u, v) derivatives1 derivatives2 =
  let first1 = VectorCurve2d.evaluateAt u derivatives1.first
      first2 = VectorCurve2d.evaluateAt v derivatives2.first
      second1 = VectorCurve2d.evaluateAt u derivatives1.second
      second2 = VectorCurve2d.evaluateAt v derivatives2.second
      first1Magnitude = Vector2d.magnitude first1
      first2Magnitude = Vector2d.magnitude first2
      second1Magnitude = Vector2d.magnitude second1
      second2Magnitude = Vector2d.magnitude second2
      first1Zero = first1Magnitude < ?tolerance
      first2Zero = first2Magnitude < ?tolerance
      tangent1 = if first1Zero then second1 / second1Magnitude else first1 / first2Magnitude
      tangent2 = if first2Zero then second2 / second2Magnitude else first2 / first2Magnitude
      tangentCrossProduct = tangent1 >< tangent2
      crossProductMagnitude = Qty.abs tangentCrossProduct
      sign0 = Qty.sign tangentCrossProduct
      radius0 = ?tolerance / crossProductMagnitude
   in if first1Zero || first2Zero
        then
          let length1 = if first1Zero then 0.5 * second1Magnitude else first1Magnitude
              length2 = if first2Zero then 0.5 * second2Magnitude else first2Magnitude
           in if crossProductMagnitude * Qty.min length1 length2 < ?tolerance
                then Error TangentIntersectionAtDegeneratePoint
                else Ok (Intersection.Crossing, sign0)
        else
          if crossProductMagnitude > 0.1
            then Ok (Intersection.Crossing, sign0)
            else
              let dX1_dU1 = first1Magnitude
                  dY1_dU1 = Qty.zero
                  dX2_dU2 = tangent1 <> first2
                  dY2_dU2 = tangent1 >< first2
                  d2X1_dU1dU1 = tangent1 <> second1
                  d2Y1_dU1dU1 = tangent1 >< second1
                  d2X2_dU2dU2 = tangent1 <> second1
                  d2Y2_dU2dU2 = tangent1 >< second1
                  d2Y1_dXdX = secondDerivative1d dX1_dU1 dY1_dU1 d2X1_dU1dU1 d2Y1_dU1dU1
                  d2Y2_dXdX = secondDerivative1d dX2_dU2 dY2_dU2 d2X2_dU2dU2 d2Y2_dU2dU2
                  d2Y_dXdX = d2Y2_dXdX - d2Y1_dXdX
                  sign1 = Qty.sign d2Y_dXdX
                  radius1 =
                    Units.specialize
                      (Qty.sqrt (2.0 * Units.generalize ?tolerance ./ Qty.abs d2Y_dXdX))
               in if radius0 <= radius1
                    then Ok (Intersection.Crossing, sign0)
                    else Ok (Intersection.Tangent, sign1)

secondDerivative1d :: Qty units -> Qty units -> Qty units -> Qty units -> Qty (Unitless :/ units)
secondDerivative1d dXdU dYdU d2XdU2 d2YdU2 =
  let dXdU' = Units.generalize dXdU
      dYdU' = Units.generalize dYdU
      d2XdU2' = Units.generalize d2XdU2
      d2YdU2' = Units.generalize d2YdU2
   in Units.generalize ((d2YdU2' * dXdU' - dYdU' * d2XdU2') / (dXdU' * dXdU')) ./ dXdU'
