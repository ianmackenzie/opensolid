module OpenSolid.SurfaceFunction1D.WithNoZeros
  ( unwrap
  , sqrt_
  , sqrt
  , squared_
  , squared
  )
where

import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Expression qualified as Expression
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D, WithNoZeros (WithNoZeros))
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.Units qualified as Units

{-# INLINE unwrap #-}
unwrap :: WithNoZeros units -> SurfaceFunction1D units
unwrap (WithNoZeros function) = function

sqrt_ :: WithNoZeros (units ?*? units) -> WithNoZeros units
sqrt_ (WithNoZeros function) = WithNoZeros do
  SurfaceFunction1D.recursive
    (CompiledFunction.map Expression.sqrt_ Quantity.sqrt_ Interval.sqrt_ (SurfaceFunction1D.compiled function))
    (\self p -> Units.coerce (0.5 *. SurfaceFunction1D.derivative p function ?/? WithNoZeros self))

sqrt :: Units.Squared units1 units2 => WithNoZeros units2 -> WithNoZeros units1
sqrt = sqrt_ . Units.unspecialize

squared_ :: WithNoZeros units -> WithNoZeros (units ?*? units)
squared_ (WithNoZeros function) = WithNoZeros (SurfaceFunction1D.squared_ function)

squared :: Units.Squared units1 units2 => WithNoZeros units1 -> WithNoZeros units2
squared = Units.specialize . squared_
