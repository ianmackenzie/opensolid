module OpenSolid.Curve1D.Nonzero
  ( sqrt_
  , sqrt
  , squared_
  , squared
  , erase
  , unerase
  )
where

import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Expression qualified as Expression
import OpenSolid.Interval qualified as Interval
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units qualified as Units

sqrt_ :: Nonzero (Curve1D (units ?*? units)) -> Nonzero (Curve1D units)
sqrt_ (Nonzero curve) = Nonzero do
  Curve1D.recursive
    (CompiledFunction.map Expression.sqrt_ Quantity.sqrt_ Interval.sqrt_ (Curve1D.compiled curve))
    (\self -> Units.coerce (0.5 * Curve1D.derivative curve ?/? Nonzero self))

sqrt :: Units.Squared units1 units2 => Nonzero (Curve1D units2) -> Nonzero (Curve1D units1)
sqrt = sqrt_ . Units.unspecialize

squared_ :: Nonzero (Curve1D units) -> Nonzero (Curve1D (units ?*? units))
squared_ (Nonzero curve) = Nonzero (Curve1D.squared_ curve)

squared :: Units.Squared units1 units2 => Nonzero (Curve1D units1) -> Nonzero (Curve1D units2)
squared = Units.specialize . squared_

erase :: Nonzero (Curve1D units) -> Nonzero (Curve1D Unitless)
erase = Units.erase

unerase :: Nonzero (Curve1D Unitless) -> Nonzero (Curve1D units)
unerase = Units.unerase
