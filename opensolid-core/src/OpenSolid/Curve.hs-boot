module OpenSolid.Curve
  ( Curve
  , Compiled
  , compiled
  , derivative
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

instance FFI (Curve Unitless)

compiled :: Curve units -> Compiled units
derivative :: Curve units -> Curve units
evaluate :: Curve units -> Float -> Qty units
evaluateBounds :: Curve units -> Bounds Unitless -> Bounds units
