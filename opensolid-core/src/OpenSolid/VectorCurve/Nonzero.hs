module OpenSolid.VectorCurve.Nonzero
  ( squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , direction
  , directionValue
  )
where

import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D.Nonzero qualified as Curve1D.Nonzero
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve

squaredMagnitude_ ::
  VectorCurve.Exists dimension units space =>
  Nonzero (VectorCurve dimension units space) ->
  Nonzero (Curve1D (units ?*? units))
squaredMagnitude_ (Nonzero curve) = Nonzero (VectorCurve.squaredMagnitude_ curve)

squaredMagnitude ::
  (VectorCurve.Exists dimension units1 space, Units.Squared units1 units2) =>
  Nonzero (VectorCurve dimension units1 space) ->
  Nonzero (Curve1D units2)
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude ::
  VectorCurve.Exists dimension units space =>
  Nonzero (VectorCurve dimension units space) ->
  Nonzero (Curve1D units)
magnitude = Curve1D.Nonzero.sqrt_ . squaredMagnitude_

direction ::
  VectorCurve.Exists dimension units space =>
  Nonzero (VectorCurve dimension units space) ->
  DirectionCurve dimension space
direction (Nonzero curve) = DirectionCurve.unsafe (curve / magnitude (Nonzero curve))

directionValue ::
  VectorCurve.Exists dimension units space =>
  Nonzero (VectorCurve dimension units space) ->
  Number ->
  Direction dimension space
directionValue (Nonzero curve) tValue = do
  let vector = VectorCurve.value curve tValue
  Direction.unsafe (vector / Vector.magnitude vector)
