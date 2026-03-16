module OpenSolid.VectorCurve.Nondegenerate
  ( magnitude
  , direction
  , squaredMagnitude_
  , squaredMagnitude
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve1D.Nondegenerate qualified as Curve1D.Nondegenerate
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

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
  , Division
      (VectorCurve dimension units space)
      (Nondegenerate (Curve1D units))
      (VectorCurve dimension Unitless space)
  ) =>
  Nondegenerate (VectorCurve dimension units space) ->
  DirectionCurve dimension space
direction (Nondegenerate curve) = DirectionCurve.unsafe (curve / magnitude (Nondegenerate curve))

squaredMagnitude ::
  ( VectorCurve.Exists dimension units1 space
  , Units.Squared units1 units2
  ) =>
  Nondegenerate (VectorCurve dimension units1 space) ->
  Nondegenerate (Curve1D units2)
squaredMagnitude = Units.specialize . squaredMagnitude_
