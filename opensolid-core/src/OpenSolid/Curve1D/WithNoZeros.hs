module OpenSolid.Curve1D.WithNoZeros
  ( unwrap
  , sqrt_
  , sqrt
  , squared_
  , squared
  )
where

import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D, WithNoZeros (WithNoZeros))
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Expression qualified as Expression
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units qualified as Units

{-# INLINE unwrap #-}
unwrap :: WithNoZeros units -> Curve1D units
unwrap (WithNoZeros curve) = curve

sqrt_ :: WithNoZeros (units ?*? units) -> WithNoZeros units
sqrt_ (WithNoZeros curve) = WithNoZeros do
  Curve1D.recursive
    (CompiledFunction.map Expression.sqrt_ Quantity.sqrt_ Interval.sqrt_ (Curve1D.compiled curve))
    (\self -> Units.coerce (0.5 *. Curve1D.derivative curve ?/? WithNoZeros self))

sqrt :: Units.Squared units1 units2 => WithNoZeros units2 -> WithNoZeros units1
sqrt = sqrt_ . Units.unspecialize

squared_ :: WithNoZeros units -> WithNoZeros (units ?*? units)
squared_ (WithNoZeros curve) = WithNoZeros (Curve1D.squared_ curve)

squared :: Units.Squared units1 units2 => WithNoZeros units1 -> WithNoZeros units2
squared = Units.specialize . squared_
