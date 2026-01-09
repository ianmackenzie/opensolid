module OpenSolid.Curve.WithNoZeros (sqrt, sqrt_) where

import {-# SOURCE #-} OpenSolid.Curve (WithNoZeros)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

sqrt_ :: WithNoZeros (units ?*? units) -> WithNoZeros units
sqrt :: Units.Squared units1 units2 => WithNoZeros units2 -> WithNoZeros units1
