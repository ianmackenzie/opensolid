module OpenSolid.SurfaceFunction1D.Nonzero
  ( sqrt_
  , sqrt
  , squared_
  , squared
  )
where

import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Expression qualified as Expression
import OpenSolid.Interval qualified as Interval
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.Units qualified as Units

sqrt_ :: Nonzero (SurfaceFunction1D (units ?*? units)) -> Nonzero (SurfaceFunction1D units)
sqrt_ (Nonzero f) = Nonzero do
  let compiledSqrt =
        CompiledFunction.map
          Expression.sqrt_
          Quantity.sqrt_
          Interval.sqrt_
          (SurfaceFunction1D.compiled f)
  let (dfdu, dfdv) = SurfaceFunction1D.partialDerivatives f
  recursive \self -> do
    let sqrtPartialDerivatives =
          Pair.map Units.coerce $
            ( 0.5 * dfdu ?/? Nonzero self
            , 0.5 * dfdv ?/? Nonzero self
            )
    SurfaceFunction1D.new compiledSqrt sqrtPartialDerivatives

sqrt ::
  Units.Squared units1 units2 =>
  Nonzero (SurfaceFunction1D units2) ->
  Nonzero (SurfaceFunction1D units1)
sqrt = sqrt_ . Units.unspecialize

squared_ :: Nonzero (SurfaceFunction1D units) -> Nonzero (SurfaceFunction1D (units ?*? units))
squared_ (Nonzero function) = Nonzero (SurfaceFunction1D.squared_ function)

squared ::
  Units.Squared units1 units2 =>
  Nonzero (SurfaceFunction1D units1) ->
  Nonzero (SurfaceFunction1D units2)
squared = Units.specialize . squared_
