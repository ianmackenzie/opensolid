module OpenSolid.Curve1D.Nondegenerate
  ( curve
  , sqrt
  , sqrt_
  , erase
  , unerase
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D, Nondegenerate)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

curve :: Nondegenerate units -> Curve1D units
sqrt_ :: Nondegenerate (units ?*? units) -> Nondegenerate units
sqrt :: Units.Squared units1 units2 => Nondegenerate units2 -> Nondegenerate units1
erase :: Nondegenerate units -> Nondegenerate Unitless
unerase :: Nondegenerate Unitless -> Nondegenerate units
