module Curve2d.Derivatives
  ( Derivatives (..)
  , ofCurve
  , AreZero (AreZero)
  , intersectionKind
  , DegenerateIntersection (DegenerateIntersection)
  , classify
  )
where

import {-# SOURCE #-} Curve2d (Curve2d)
import {-# SOURCE #-} Curve2d qualified
import Curve2d.Intersection qualified as Intersection
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified
import Units (Unitless, (:/))
import Units qualified
import Vector2d qualified
import VectorBox2d (VectorBox2d)
import VectorBox2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

data AreZero = AreZero deriving (Show)

instance IsError AreZero where
  errorMessage AreZero = "First and second derivatives are simultaneously zero"

data Derivatives (coordinateSystem :: CoordinateSystem) = Derivatives
  { curve :: Curve2d coordinateSystem
  , first :: VectorCurve2d coordinateSystem
  , second :: VectorCurve2d coordinateSystem
  , degenerateStart :: Bool
  , degenerateEnd :: Bool
  }

ofCurve :: Tolerance units => Curve2d (space @ units) -> Result AreZero (Derivatives (space @ units))
ofCurve givenCurve =
  let firstDerivative = Curve2d.derivative givenCurve
      secondDerivative = VectorCurve2d.derivative firstDerivative
   in if simultaneouslyZero firstDerivative secondDerivative
        then Error AreZero
        else
          Ok $
            Derivatives
              { curve = givenCurve
              , first = firstDerivative
              , second = secondDerivative
              , degenerateStart = isDegenerate firstDerivative 0.0
              , degenerateEnd = isDegenerate firstDerivative 1.0
              }

simultaneouslyZero
  :: Tolerance units
  => VectorCurve2d (space @ units)
  -> VectorCurve2d (space @ units)
  -> Bool
simultaneouslyZero firstDerivative secondDerivative =
  Range.any (areBothZero firstDerivative secondDerivative) Range.unit

areBothZero
  :: Tolerance units
  => VectorCurve2d (space @ units)
  -> VectorCurve2d (space @ units)
  -> Range Unitless
  -> Fuzzy Bool
areBothZero firstDerivative secondDerivative domain
  | firstMin > squaredTolerance || secondMin > 4.0 * squaredTolerance = Resolved False
  | firstMax < squaredTolerance && secondMax < 4.0 * squaredTolerance = Resolved True
  | otherwise = Unresolved
 where
  firstBounds = VectorCurve2d.segmentBounds firstDerivative domain
  secondBounds = VectorCurve2d.segmentBounds secondDerivative domain
  firstSquaredMagnitude = VectorBox2d.squaredMagnitude (Units.generalize firstBounds)
  secondSquaredMagnitude = VectorBox2d.squaredMagnitude (Units.generalize secondBounds)
  firstMin = Range.minValue firstSquaredMagnitude
  firstMax = Range.maxValue firstSquaredMagnitude
  secondMin = Range.minValue secondSquaredMagnitude
  secondMax = Range.maxValue secondSquaredMagnitude
  squaredTolerance = Qty.squared (Units.generalize ?tolerance)

isDegenerate :: Tolerance units => VectorCurve2d (space @ units) -> Float -> Bool
isDegenerate firstDerivative u =
  Vector2d.magnitude (VectorCurve2d.evaluate firstDerivative u) ~= Qty.zero

tangentBounds
  :: Derivatives (space @ units)
  -> Range Unitless
  -> VectorBox2d (space @ units)
  -> VectorBox2d (space @ units)
  -> VectorBox2d (space @ Unitless)
tangentBounds derivatives u firstBounds secondBounds
  | Range.includes 0.0 u && derivatives.degenerateStart = VectorBox2d.normalize secondBounds
  | Range.includes 1.0 u && derivatives.degenerateEnd = -(VectorBox2d.normalize secondBounds)
  | otherwise = VectorBox2d.normalize firstBounds

intersectionKind
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Range Unitless
  -> Range Unitless
  -> Fuzzy (Maybe Intersection.Kind)
intersectionKind derivatives1 derivatives2 u1 u2 =
  let curveBounds1 = Curve2d.segmentBounds derivatives1.curve u1
      curveBounds2 = Curve2d.segmentBounds derivatives2.curve u2
      difference = curveBounds1 - curveBounds2
      distance = VectorBox2d.magnitude difference
   in if Range.minValue distance > ?tolerance
        then Resolved Nothing
        else
          let firstBounds1 = VectorCurve2d.segmentBounds derivatives1.first u1
              firstBounds2 = VectorCurve2d.segmentBounds derivatives2.first u2
              secondBounds1 = VectorCurve2d.segmentBounds derivatives1.second u1
              secondBounds2 = VectorCurve2d.segmentBounds derivatives2.second u2
              tangentBounds1 = tangentBounds derivatives1 u1 firstBounds1 secondBounds1
              tangentBounds2 = tangentBounds derivatives2 u2 firstBounds2 secondBounds2
              firstResolution = Range.resolution (tangentBounds1 >< tangentBounds2)
           in if Qty.abs firstResolution >= 0.5
                then Resolved (Just (Intersection.Crossing (Qty.sign firstResolution)))
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
                        then Resolved (Just (Intersection.Tangent (Qty.sign secondResolution)))
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

data DegenerateIntersection = DegenerateIntersection deriving (Show)

instance IsError DegenerateIntersection where
  errorMessage DegenerateIntersection =
    "Tangent intersection where a curve first derivative is zero"

classify
  :: Tolerance units
  => Derivatives (space @ units)
  -> Derivatives (space @ units)
  -> Float
  -> Float
  -> Result DegenerateIntersection Intersection.Kind
classify derivatives1 derivatives2 u1 u2 =
  let first1 = VectorCurve2d.evaluate derivatives1.first u1
      first2 = VectorCurve2d.evaluate derivatives2.first u2
      second1 = VectorCurve2d.evaluate derivatives1.second u1
      second2 = VectorCurve2d.evaluate derivatives2.second u2
      first1Magnitude = Vector2d.magnitude first1
      first2Magnitude = Vector2d.magnitude first2
      second1Magnitude = Vector2d.magnitude second1
      second2Magnitude = Vector2d.magnitude second2
      first1Zero = first1Magnitude < ?tolerance
      first2Zero = first2Magnitude < ?tolerance
      tangent1 = if first1Zero then second1 / second1Magnitude else first1 / first2Magnitude
      tangent2 = if first2Zero then second2 / second2Magnitude else first2 / first2Magnitude
      tangentCrossProduct = tangent1 >< tangent2
      crossProductSign = Qty.sign tangentCrossProduct
      crossProductMagnitude = Qty.abs tangentCrossProduct
   in if first1Zero || first2Zero
        then
          let length1 = if first1Zero then 0.5 * second1Magnitude else first1Magnitude
              length2 = if first2Zero then 0.5 * second2Magnitude else first2Magnitude
           in if crossProductMagnitude * min length1 length2 < ?tolerance
                then Error DegenerateIntersection
                else Ok (Intersection.Crossing crossProductSign)
        else
          if crossProductMagnitude > 0.5
            then Ok (Intersection.Crossing crossProductSign)
            else
              let width0 = Qty.abs (?tolerance / tangentCrossProduct)
                  dX1_dU1 = first1Magnitude
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
                  width1 = Units.specialize (Qty.sqrt (2.0 * Units.generalize ?tolerance ./ d2Y_dXdX))
               in if width0 <= width1
                    then Ok (Intersection.Crossing crossProductSign)
                    else Ok (Intersection.Tangent sign1)

secondDerivative1d :: Qty units -> Qty units -> Qty units -> Qty units -> Qty (Unitless :/ units)
secondDerivative1d dXdU dYdU d2XdU2 d2YdU2 =
  let dXdU' = Units.generalize dXdU
      dYdU' = Units.generalize dYdU
      d2XdU2' = Units.generalize d2XdU2
      d2YdU2' = Units.generalize d2YdU2
   in Units.generalize ((d2YdU2' * dXdU' - dYdU' * d2XdU2') / (dXdU' * dXdU')) ./ dXdU'
