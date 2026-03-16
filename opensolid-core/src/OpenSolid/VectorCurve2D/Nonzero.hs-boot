module OpenSolid.VectorCurve2D.Nonzero
  ( squaredMagnitude
  , squaredMagnitude_
  , magnitude
  )
where

import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Nonzero (Nonzero)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

squaredMagnitude_ :: Nonzero (VectorCurve2D units space) -> Nonzero (Curve1D (units ?*? units))
squaredMagnitude ::
  Units.Squared units1 units2 =>
  Nonzero (VectorCurve2D units1 space) ->
  Nonzero (Curve1D units2)
magnitude :: Nonzero (VectorCurve2D units space) -> Nonzero (Curve1D units)
