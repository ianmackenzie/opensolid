module OpenSolid.SurfaceFunction1D.Nonzero
  ( sqrt
  , sqrt_
  , squared
  , squared_
  )
where

import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.Units qualified as Units

sqrt_ :: Nonzero (SurfaceFunction1D (units ?*? units)) -> Nonzero (SurfaceFunction1D units)
sqrt ::
  Units.Squared units1 units2 =>
  Nonzero (SurfaceFunction1D units2) ->
  Nonzero (SurfaceFunction1D units1)
squared_ :: Nonzero (SurfaceFunction1D units) -> Nonzero (SurfaceFunction1D (units ?*? units))
squared ::
  Units.Squared units1 units2 =>
  Nonzero (SurfaceFunction1D units1) ->
  Nonzero (SurfaceFunction1D units2)
