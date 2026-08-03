module OpenSolid.Curve1D.Nondegenerate
  ( squared
  , squared_
  , erase
  , unerase
  )
where

import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

squared_ :: Nondegenerate (Curve1D units) -> Nondegenerate (Curve1D (units ?*? units))
squared_ (Nondegenerate curve) = Nondegenerate (Curve1D.squared_ curve)

squared ::
  Units.Squared units1 units2 =>
  Nondegenerate (Curve1D units1) ->
  Nondegenerate (Curve1D units2)
squared = Units.specialize . squared_

erase :: Nondegenerate (Curve1D units) -> Nondegenerate (Curve1D Unitless)
erase = Units.erase

unerase :: Nondegenerate (Curve1D Unitless) -> Nondegenerate (Curve1D units)
unerase = Units.unerase
