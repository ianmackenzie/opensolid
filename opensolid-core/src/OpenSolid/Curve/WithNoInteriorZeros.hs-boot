module OpenSolid.Curve.WithNoInteriorZeros (unwrap, sqrt, sqrt_) where

import {-# SOURCE #-} OpenSolid.Curve (Curve, WithNoInteriorZeros)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

unwrap :: WithNoInteriorZeros units -> Curve units
sqrt_ :: WithNoInteriorZeros (units ?*? units) -> WithNoInteriorZeros units
sqrt :: Units.Squared units1 units2 => WithNoInteriorZeros units2 -> WithNoInteriorZeros units1
