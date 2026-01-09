module OpenSolid.Curve.WithNoZeros (unwrap, sqrt_, sqrt) where

import OpenSolid.Bounds qualified as Bounds
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve (Curve, WithNoZeros (WithNoZeros))
import OpenSolid.Curve qualified as Curve
import OpenSolid.Expression qualified as Expression
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units qualified as Units

{-# INLINE unwrap #-}
unwrap :: WithNoZeros units -> Curve units
unwrap (WithNoZeros curve) = curve

sqrt_ :: WithNoZeros (units ?*? units) -> WithNoZeros units
sqrt_ (WithNoZeros curve) = WithNoZeros do
  Curve.recursive
    (CompiledFunction.map Expression.sqrt_ Quantity.sqrt_ Bounds.sqrt_ (Curve.compiled curve))
    (\self -> Units.coerce (0.5 *. Curve.derivative curve ?/? WithNoZeros self))

sqrt :: Units.Squared units1 units2 => WithNoZeros units2 -> WithNoZeros units1
sqrt = sqrt_ . Units.unspecialize
