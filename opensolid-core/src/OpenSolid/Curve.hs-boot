module OpenSolid.Curve
  ( Curve
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.FFI (FFI)
import OpenSolid.Prelude

type role Curve nominal

type Curve :: Type -> Type
data Curve units

instance FFI (Curve Unitless)

evaluate :: Curve units -> Float -> Qty units
evaluateBounds :: Curve units -> Bounds Unitless -> Bounds units
derivative :: Curve units -> Curve units
