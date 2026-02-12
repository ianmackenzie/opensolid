module OpenSolid.Curve1D.WithNoInteriorZeros
  ( unwrap
  , sqrt
  , sqrt_
  , erase
  , unerase
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D, WithNoInteriorZeros)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

unwrap :: WithNoInteriorZeros units -> Curve1D units
sqrt_ :: WithNoInteriorZeros (units ?*? units) -> WithNoInteriorZeros units
sqrt :: Units.Squared units1 units2 => WithNoInteriorZeros units2 -> WithNoInteriorZeros units1
erase :: WithNoInteriorZeros units -> WithNoInteriorZeros Unitless
unerase :: WithNoInteriorZeros Unitless -> WithNoInteriorZeros units
