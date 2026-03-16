module OpenSolid.Curve1D.Nondegenerate
  ( squared
  , squared_
  , sqrt
  , sqrt_
  , erase
  , unerase
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

squared_ :: Nondegenerate (Curve1D units) -> Nondegenerate (Curve1D (units ?*? units))
squared :: Units.Squared units1 units2 => Nondegenerate (Curve1D units1) -> Nondegenerate (Curve1D units2)
sqrt_ :: Nondegenerate (Curve1D (units ?*? units)) -> Nondegenerate (Curve1D units)
sqrt :: Units.Squared units1 units2 => Nondegenerate (Curve1D units2) -> Nondegenerate (Curve1D units1)
erase :: Nondegenerate (Curve1D units) -> Nondegenerate (Curve1D Unitless)
unerase :: Nondegenerate (Curve1D Unitless) -> Nondegenerate (Curve1D units)
