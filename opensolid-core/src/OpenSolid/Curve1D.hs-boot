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
import OpenSolid.CurveParameter (CurveParameter)
import OpenSolid.Differentiable (Differentiable)
import OpenSolid.FFI (FFI)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

type role Curve1D nominal

type Curve1D :: Type -> Type
data Curve1D units = Curve1D {compiled :: Compiled units, derivative :: ~(Curve1D units)}

instance Differentiable CurveParameter (Curve1D units) (Curve1D units)

instance Composition (Curve1D Unitless) (Curve1D units) (Curve1D units)

type Compiled units = CompiledFunction Number (Quantity units) (Interval Unitless) (Interval units)

instance FFI (Curve1D Unitless)

newtype WithNoZeros units = WithNoZeros (Curve1D units)

newtype WithNoInteriorZeros units = WithNoInteriorZeros (Curve1D units)

instance units1 ~ units2 => Addition (Curve1D units1) (Curve1D units2) (Curve1D units1)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (Quantity units2) (Curve1D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Curve1D units2) (Curve1D units3)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1D units1) (Curve1D units2) (Curve1D units3)

evaluate :: Curve1D units -> Number -> Quantity units
evaluateBounds :: Curve1D units -> Interval Unitless -> Interval units
