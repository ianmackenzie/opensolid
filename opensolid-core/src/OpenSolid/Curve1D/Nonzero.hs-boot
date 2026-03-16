module OpenSolid.Curve1D.Nonzero
  ( sqrt
  , sqrt_
  , squared
  , squared_
  , erase
  , unerase
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

sqrt_ :: Nonzero (Curve1D (units ?*? units)) -> Nonzero (Curve1D units)
sqrt :: Units.Squared units1 units2 => Nonzero (Curve1D units2) -> Nonzero (Curve1D units1)
squared_ :: Nonzero (Curve1D units) -> Nonzero (Curve1D (units ?*? units))
squared :: Units.Squared units1 units2 => Nonzero (Curve1D units1) -> Nonzero (Curve1D units2)
erase :: Nonzero (Curve1D units) -> Nonzero (Curve1D Unitless)
unerase :: Nonzero (Curve1D Unitless) -> Nonzero (Curve1D units)
