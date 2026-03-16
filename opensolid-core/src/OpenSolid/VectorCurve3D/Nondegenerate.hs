module OpenSolid.VectorCurve3D.Nondegenerate
  ( Nondegenerate
  , magnitude
  , normalize
  , direction
  , squaredMagnitude_
  , squaredMagnitude
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve1D.Nondegenerate qualified as Curve1D.Nondegenerate
import {-# SOURCE #-} OpenSolid.DirectionCurve3D (DirectionCurve3D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D qualified as DirectionCurve3D
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D qualified as VectorCurve3D

magnitude :: Nondegenerate (VectorCurve3D units space) -> Nondegenerate (Curve1D units)
magnitude = Curve1D.Nondegenerate.sqrt_ . squaredMagnitude_

normalize :: Nondegenerate (VectorCurve3D units space) -> VectorCurve3D Unitless space
normalize (Nondegenerate curve) = curve / magnitude (Nondegenerate curve)

squaredMagnitude_ ::
  Nondegenerate (VectorCurve3D units space) ->
  Nondegenerate (Curve1D (units ?*? units))
squaredMagnitude_ (Nondegenerate curve) = Nondegenerate (VectorCurve3D.squaredMagnitude_ curve)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Nondegenerate (VectorCurve3D units1 space) ->
  Nondegenerate (Curve1D units2)
squaredMagnitude = Units.specialize . squaredMagnitude_

direction :: Nondegenerate (VectorCurve3D units space) -> DirectionCurve3D space
direction = DirectionCurve3D.unsafe . normalize
