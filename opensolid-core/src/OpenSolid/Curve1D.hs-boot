module OpenSolid.Curve1D
  ( Curve1D (compiled, derivative)
  , Compiled
  , WithNoInteriorZeros (WithNoInteriorZeros)
  , WithNoZeros (WithNoZeros)
  , evaluate
  , evaluateBounds
  )
where

import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.FFI (FFI)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude

type role Curve1D nominal

type Curve1D :: Type -> Type
data Curve1D units = Curve1D {compiled :: Compiled units, derivative :: ~(Curve1D units)}

type Compiled units = CompiledFunction Number (Quantity units) (Interval Unitless) (Interval units)

instance FFI (Curve1D Unitless)

newtype WithNoZeros units = WithNoZeros (Curve1D units)

newtype WithNoInteriorZeros units = WithNoInteriorZeros (Curve1D units)

evaluate :: Curve1D units -> Number -> Quantity units
evaluateBounds :: Curve1D units -> Interval Unitless -> Interval units
