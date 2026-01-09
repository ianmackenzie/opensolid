module OpenSolid.Curve.WithNoInteriorZeros (unwrap, sqrt, sqrt_) where

import OpenSolid.Curve (Curve, WithNoInteriorZeros (WithNoInteriorZeros), WithNoZeros (WithNoZeros))
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.WithNoZeros qualified as Curve.WithNoZeros
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units

{-# INLINE unwrap #-}
unwrap :: WithNoInteriorZeros units -> Curve units
unwrap (WithNoInteriorZeros curve) = curve

sqrt_ :: WithNoInteriorZeros (units ?*? units) -> WithNoInteriorZeros units
sqrt_ (WithNoInteriorZeros curve) = WithNoInteriorZeros do
  let firstDerivative = Curve.derivative curve
  let secondDerivative = Curve.derivative firstDerivative
  let curveTolerance = Curve.singularityTolerance curve
  let firstDerivativeTolerance = Curve.singularityTolerance firstDerivative
  let singularity tValue sign =
        (Quantity.zero, sign .*. Quantity.sqrt_ (0.5 *. Curve.evaluate secondDerivative tValue))
  let maybeSingularity tValue sign = do
        let curveIsZero = Tolerance.using curveTolerance do
              Curve.evaluate curve tValue ~= Quantity.zero
        let firstDerivativeIsZero = Tolerance.using firstDerivativeTolerance do
              Curve.evaluate firstDerivative tValue ~= Quantity.zero
        if curveIsZero && firstDerivativeIsZero then Just (singularity tValue sign) else Nothing
  let WithNoZeros interiorSqrt = Curve.WithNoZeros.sqrt_ (WithNoZeros curve)
  Curve.desingularize (maybeSingularity 0 Positive) interiorSqrt (maybeSingularity 1 Negative)

sqrt :: Units.Squared units1 units2 => WithNoInteriorZeros units2 -> WithNoInteriorZeros units1
sqrt = sqrt_ . Units.unspecialize
