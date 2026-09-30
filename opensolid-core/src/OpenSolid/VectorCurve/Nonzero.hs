module OpenSolid.VectorCurve.Nonzero
  ( squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , normalize
  )
where

import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D.Nonzero qualified as Curve1D.Nonzero
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve (VectorCurve, VectorCurveExists)
import OpenSolid.VectorCurve qualified as VectorCurve

squaredMagnitude_ ::
  VectorCurveExists dimension units space =>
  Nonzero (VectorCurve dimension units space) ->
  Nonzero (Curve1D (units ?*? units))
squaredMagnitude_ (Nonzero curve) = Nonzero (VectorCurve.squaredMagnitude_ curve)

squaredMagnitude ::
  (VectorCurveExists dimension units1 space, Units.Squared units1 units2) =>
  Nonzero (VectorCurve dimension units1 space) ->
  Nonzero (Curve1D units2)
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude ::
  VectorCurveExists dimension units space =>
  Nonzero (VectorCurve dimension units space) ->
  Nonzero (Curve1D units)
magnitude = Curve1D.Nonzero.sqrt_ . squaredMagnitude_

normalize ::
  VectorCurveExists dimension units space =>
  Nonzero (VectorCurve dimension units space) ->
  Nonzero (VectorCurve dimension Unitless space)
normalize (Nonzero curve) = Nonzero (curve / magnitude (Nonzero curve))
