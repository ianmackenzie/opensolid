module OpenSolid.VectorCurve.Nondegenerate
  ( direction
  , squaredMagnitude_
  , squaredMagnitude
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve

squaredMagnitude_ ::
  VectorCurve.Exists dimension units space =>
  Nondegenerate (VectorCurve dimension units space) ->
  Nondegenerate (Curve1D (units ?*? units))
squaredMagnitude_ (Nondegenerate curve) = Nondegenerate (VectorCurve.squaredMagnitude_ curve)

direction ::
  (VectorCurve.Exists dimension units space, Direction.Exists dimension space) =>
  Nondegenerate (VectorCurve dimension units space) ->
  Number ->
  Direction dimension space
direction (Nondegenerate curve) tValue =
  Direction.unsafe $
    Tolerance.using Quantity.zero $
      Vector.normalize $
        if
          | tValue == 0.0 && VectorCurve.hasDegenerateStart curve ->
              VectorCurve.derivativeAt 0.0 curve
          | tValue == 1.0 && VectorCurve.hasDegenerateEnd curve ->
              negate (VectorCurve.derivativeAt 1.0 curve)
          | otherwise -> VectorCurve.valueAt tValue curve

squaredMagnitude ::
  ( VectorCurve.Exists dimension units1 space
  , Units.Squared units1 units2
  ) =>
  Nondegenerate (VectorCurve dimension units1 space) ->
  Nondegenerate (Curve1D units2)
squaredMagnitude = Units.specialize . squaredMagnitude_
