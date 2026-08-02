module OpenSolid.VectorCurve.Nonzero
  ( valueAt
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , normalize
  , directionAt
  )
where

import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D.Nonzero qualified as Curve1D.Nonzero
import OpenSolid.Direction (Direction)
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.Vector.Nonzero qualified as Vector.Nonzero
import OpenSolid.VectorCurve (VectorCurve, VectorCurveExists)
import OpenSolid.VectorCurve qualified as VectorCurve

valueAt ::
  VectorCurveExists dimension units space =>
  Number ->
  Nonzero (VectorCurve dimension units space) ->
  Nonzero (Vector dimension units space)
valueAt tValue (Nonzero curve) = Nonzero (VectorCurve.valueAt tValue curve)

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

directionAt ::
  VectorCurveExists dimension units space =>
  Number ->
  Nonzero (VectorCurve dimension units space) ->
  Direction dimension space
directionAt tValue curve = Vector.Nonzero.direction (valueAt tValue curve)
