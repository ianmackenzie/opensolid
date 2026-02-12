module OpenSolid.Curve1D.WithNoZeros
  ( unwrap
  , sqrt
  , sqrt_
  , squared
  , squared_
  , erase
  , unerase
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D, WithNoZeros)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

unwrap :: WithNoZeros units -> Curve1D units
sqrt_ :: WithNoZeros (units ?*? units) -> WithNoZeros units
sqrt :: Units.Squared units1 units2 => WithNoZeros units2 -> WithNoZeros units1
squared_ :: WithNoZeros units -> WithNoZeros (units ?*? units)
squared :: Units.Squared units1 units2 => WithNoZeros units1 -> WithNoZeros units2
erase :: WithNoZeros units -> WithNoZeros Unitless
unerase :: WithNoZeros Unitless -> WithNoZeros units
