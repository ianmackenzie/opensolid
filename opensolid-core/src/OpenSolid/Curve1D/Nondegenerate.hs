module OpenSolid.Curve1D.Nondegenerate
  ( squared
  , squared_
  , sqrt
  , sqrt_
  , erase
  , unerase
  )
where

import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Nonzero qualified as Curve1D.Nonzero
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units

squared_ :: Nondegenerate (Curve1D units) -> Nondegenerate (Curve1D (units ?*? units))
squared_ (Nondegenerate curve) = Nondegenerate (Curve1D.squared_ curve)

squared ::
  Units.Squared units1 units2 =>
  Nondegenerate (Curve1D units1) ->
  Nondegenerate (Curve1D units2)
squared = Units.specialize . squared_

sqrt_ :: Nondegenerate (Curve1D (units ?*? units)) -> Nondegenerate (Curve1D units)
sqrt_ (Nondegenerate curve) = Nondegenerate do
  let curveTolerance = Curve1D.singularityTolerance curve
  let derivativeTolerance = Curve1D.singularityTolerance (Curve1D.derivative curve)
  let singularity tValue sign =
        ( Quantity.zero
        , sign * Quantity.sqrt_ (0.5 * Curve1D.secondDerivativeValue curve tValue)
        )
  let maybeSingularity tValue sign = do
        let curveIsZero = Tolerance.using curveTolerance do
              Curve1D.value curve tValue ~= Quantity.zero
        let derivativeIsZero = Tolerance.using derivativeTolerance do
              Curve1D.derivativeValue curve tValue ~= Quantity.zero
        if curveIsZero && derivativeIsZero then Just (singularity tValue sign) else Nothing
  let Nonzero interiorSqrt = Curve1D.Nonzero.sqrt_ (Nonzero curve)
  Curve1D.desingularize (maybeSingularity 0.0 Positive) interiorSqrt (maybeSingularity 1.0 Negative)

sqrt ::
  Units.Squared units1 units2 =>
  Nondegenerate (Curve1D units2) ->
  Nondegenerate (Curve1D units1)
sqrt = sqrt_ . Units.unspecialize

erase :: Nondegenerate (Curve1D units) -> Nondegenerate (Curve1D Unitless)
erase = Units.erase

unerase :: Nondegenerate (Curve1D Unitless) -> Nondegenerate (Curve1D units)
unerase = Units.unerase
