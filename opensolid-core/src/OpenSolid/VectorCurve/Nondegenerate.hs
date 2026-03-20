module OpenSolid.VectorCurve.Nondegenerate
  ( magnitude
  , direction
  , directionValue
  , squaredMagnitude_
  , squaredMagnitude
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve1D.Nondegenerate qualified as Curve1D.Nondegenerate
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
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

magnitude ::
  VectorCurve.Exists dimension units space =>
  Nondegenerate (VectorCurve dimension units space) ->
  Nondegenerate (Curve1D units)
magnitude = Curve1D.Nondegenerate.sqrt_ . squaredMagnitude_

direction ::
  ( VectorCurve.Exists dimension units space
  , DirectionCurve.Exists dimension space
  ) =>
  Nondegenerate (VectorCurve dimension units space) ->
  DirectionCurve dimension space
direction (Nondegenerate curve) = DirectionCurve.unsafe (curve / magnitude (Nondegenerate curve))

directionValue ::
  (VectorCurve.Exists dimension units space, Direction.Exists dimension space) =>
  Nondegenerate (VectorCurve dimension units space) ->
  Number ->
  Direction dimension space
directionValue (Nondegenerate curve) tValue =
  Direction.unsafe $
    Tolerance.using Quantity.zero $
      Vector.normalize $
        if
          | tValue == 0.0 && VectorCurve.singular0 curve ->
              VectorCurve.derivativeValue curve 0.0
          | tValue == 1.0 && VectorCurve.singular1 curve ->
              negate (VectorCurve.derivativeValue curve 1.0)
          | otherwise -> VectorCurve.value curve tValue

squaredMagnitude ::
  ( VectorCurve.Exists dimension units1 space
  , Units.Squared units1 units2
  ) =>
  Nondegenerate (VectorCurve dimension units1 space) ->
  Nondegenerate (Curve1D units2)
squaredMagnitude = Units.specialize . squaredMagnitude_
