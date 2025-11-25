module OpenSolid.Curve
  ( Curve (compiled, derivative)
  , Compiled
  , evaluate
  , evaluateBounds
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.FFI (FFI)
import OpenSolid.Prelude

type role Curve nominal

type Curve :: Type -> Type
data Curve units = Curve {compiled :: Compiled units, derivative :: ~(Curve units)}

type Compiled units = CompiledFunction Number (Quantity units) (Bounds Unitless) (Bounds units)

instance FFI (Curve Unitless)

evaluate :: Curve units -> Number -> Quantity units
evaluateBounds :: Curve units -> Bounds Unitless -> Bounds units
