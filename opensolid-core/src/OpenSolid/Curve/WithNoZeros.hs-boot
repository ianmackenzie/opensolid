module OpenSolid.Curve.WithNoZeros
  ( unwrap
  , sqrt
  , sqrt_
  , squared
  , squared_
  )
where

import {-# SOURCE #-} OpenSolid.Curve (Curve, WithNoZeros)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

unwrap :: WithNoZeros units -> Curve units
sqrt_ :: WithNoZeros (units ?*? units) -> WithNoZeros units
sqrt :: Units.Squared units1 units2 => WithNoZeros units2 -> WithNoZeros units1
squared_ :: WithNoZeros units -> WithNoZeros (units ?*? units)
squared :: Units.Squared units1 units2 => WithNoZeros units1 -> WithNoZeros units2
