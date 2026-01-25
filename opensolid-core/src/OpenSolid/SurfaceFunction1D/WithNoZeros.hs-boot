module OpenSolid.SurfaceFunction1D.WithNoZeros
  ( unwrap
  , sqrt
  , sqrt_
  , squared
  , squared_
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D, WithNoZeros)
import OpenSolid.Units qualified as Units

unwrap :: WithNoZeros units -> SurfaceFunction1D units
sqrt_ :: WithNoZeros (units ?*? units) -> WithNoZeros units
sqrt :: Units.Squared units1 units2 => WithNoZeros units2 -> WithNoZeros units1
squared_ :: WithNoZeros units -> WithNoZeros (units ?*? units)
squared :: Units.Squared units1 units2 => WithNoZeros units1 -> WithNoZeros units2
