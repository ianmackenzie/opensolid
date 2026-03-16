module OpenSolid.VectorCurve3D.Nonzero
  ( squaredMagnitude
  , squaredMagnitude_
  , magnitude
  )
where

import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)

squaredMagnitude_ :: Nonzero (VectorCurve3D units space) -> Nonzero (Curve1D (units ?*? units))
squaredMagnitude ::
  Units.Squared units1 units2 =>
  Nonzero (VectorCurve3D units1 space) ->
  Nonzero (Curve1D units2)
magnitude :: Nonzero (VectorCurve3D units space) -> Nonzero (Curve1D units)
