module OpenSolid.Curve
  ( Curve
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
data Curve units

type Compiled units = CompiledFunction Float (Qty units) (Bounds Unitless) (Bounds units)

instance HasField "compiled" (Curve units) (Compiled units)

instance HasField "derivative" (Curve units) (Curve units)

instance FFI (Curve Unitless)

evaluate :: Curve units -> Float -> Qty units
evaluateBounds :: Curve units -> Bounds Unitless -> Bounds units
