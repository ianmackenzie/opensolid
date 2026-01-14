module OpenSolid.Curve1D.WithNoInteriorZeros (unwrap, sqrt, sqrt_) where

import OpenSolid.Curve1D (Curve1D, WithNoInteriorZeros (WithNoInteriorZeros), WithNoZeros (WithNoZeros))
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.WithNoZeros qualified as Curve1D.WithNoZeros
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units

{-# INLINE unwrap #-}
unwrap :: WithNoInteriorZeros units -> Curve1D units
unwrap (WithNoInteriorZeros curve) = curve

sqrt_ :: WithNoInteriorZeros (units ?*? units) -> WithNoInteriorZeros units
sqrt_ (WithNoInteriorZeros curve) = WithNoInteriorZeros do
  let firstDerivative = Curve1D.derivative curve
  let secondDerivative = Curve1D.derivative firstDerivative
  let curveTolerance = Curve1D.singularityTolerance curve
  let firstDerivativeTolerance = Curve1D.singularityTolerance firstDerivative
  let singularity tValue sign =
        (Quantity.zero, sign .*. Quantity.sqrt_ (0.5 *. Curve1D.evaluate secondDerivative tValue))
  let maybeSingularity tValue sign = do
        let curveIsZero = Tolerance.using curveTolerance do
              Curve1D.evaluate curve tValue ~= Quantity.zero
        let firstDerivativeIsZero = Tolerance.using firstDerivativeTolerance do
              Curve1D.evaluate firstDerivative tValue ~= Quantity.zero
        if curveIsZero && firstDerivativeIsZero then Just (singularity tValue sign) else Nothing
  let WithNoZeros interiorSqrt = Curve1D.WithNoZeros.sqrt_ (WithNoZeros curve)
  Curve1D.desingularize (maybeSingularity 0 Positive) interiorSqrt (maybeSingularity 1 Negative)

sqrt :: Units.Squared units1 units2 => WithNoInteriorZeros units2 -> WithNoInteriorZeros units1
sqrt = sqrt_ . Units.unspecialize
