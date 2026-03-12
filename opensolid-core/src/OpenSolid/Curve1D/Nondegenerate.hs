module OpenSolid.Curve1D.Nondegenerate
  ( curve
  , squared
  , squared_
  , sqrt
  , sqrt_
  , erase
  , unerase
  )
where

import OpenSolid.Curve1D (Curve1D, Nondegenerate (Nondegenerate), WithNoZeros (WithNoZeros))
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.WithNoZeros qualified as Curve1D.WithNoZeros
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units

{-# INLINE curve #-}
curve :: Nondegenerate units -> Curve1D units
curve (Nondegenerate c) = c

squared_ :: Nondegenerate units -> Nondegenerate (units ?*? units)
squared_ (Nondegenerate givenCurve) = Nondegenerate (Curve1D.squared_ givenCurve)

squared :: Units.Squared units1 units2 => Nondegenerate units1 -> Nondegenerate units2
squared = Units.specialize . squared_

sqrt_ :: Nondegenerate (units ?*? units) -> Nondegenerate units
sqrt_ (Nondegenerate givenCurve) = Nondegenerate do
  let firstDerivative = Curve1D.derivative givenCurve
  let secondDerivative = Curve1D.derivative firstDerivative
  let curveTolerance = Curve1D.singularityTolerance givenCurve
  let firstDerivativeTolerance = Curve1D.singularityTolerance firstDerivative
  let singularity tValue sign =
        (Quantity.zero, sign * Quantity.sqrt_ (0.5 * Curve1D.evaluate secondDerivative tValue))
  let maybeSingularity tValue sign = do
        let curveIsZero = Tolerance.using curveTolerance do
              Curve1D.evaluate givenCurve tValue ~= Quantity.zero
        let firstDerivativeIsZero = Tolerance.using firstDerivativeTolerance do
              Curve1D.evaluate firstDerivative tValue ~= Quantity.zero
        if curveIsZero && firstDerivativeIsZero then Just (singularity tValue sign) else Nothing
  let WithNoZeros interiorSqrt = Curve1D.WithNoZeros.sqrt_ (WithNoZeros givenCurve)
  Curve1D.desingularize (maybeSingularity 0.0 Positive) interiorSqrt (maybeSingularity 1.0 Negative)

sqrt :: Units.Squared units1 units2 => Nondegenerate units2 -> Nondegenerate units1
sqrt = sqrt_ . Units.unspecialize

erase :: Nondegenerate units -> Nondegenerate Unitless
erase = Units.erase

unerase :: Nondegenerate Unitless -> Nondegenerate units
unerase = Units.unerase
