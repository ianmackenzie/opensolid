module OpenSolid.VectorCurve2D.Nonzero
  ( squaredMagnitude
  , squaredMagnitude_
  , magnitude
  )
where

import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D.Nonzero qualified as Curve1D.Nonzero
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

squaredMagnitude_ :: Nonzero (VectorCurve2D units space) -> Nonzero (Curve1D (units ?*? units))
squaredMagnitude_ (Nonzero curve) = Nonzero (VectorCurve2D.squaredMagnitude_ curve)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Nonzero (VectorCurve2D units1 space) ->
  Nonzero (Curve1D units2)
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude :: Nonzero (VectorCurve2D units space) -> Nonzero (Curve1D units)
magnitude = Curve1D.Nonzero.sqrt_ . squaredMagnitude_
