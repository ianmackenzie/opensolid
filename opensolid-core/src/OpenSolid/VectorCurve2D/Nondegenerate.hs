module OpenSolid.VectorCurve2D.Nondegenerate
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
import {-# SOURCE #-} OpenSolid.DirectionCurve2D (DirectionCurve2D)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)
import {-# SOURCE #-} OpenSolid.VectorCurve2D qualified as VectorCurve2D

magnitude :: Nondegenerate (VectorCurve2D units space) -> Nondegenerate (Curve1D units)
magnitude = Curve1D.Nondegenerate.sqrt_ . squaredMagnitude_

normalize :: Nondegenerate (VectorCurve2D units space) -> VectorCurve2D Unitless space
normalize (Nondegenerate curve) = curve / magnitude (Nondegenerate curve)

squaredMagnitude_ ::
  Nondegenerate (VectorCurve2D units space) ->
  Nondegenerate (Curve1D (units ?*? units))
squaredMagnitude_ (Nondegenerate curve) = Nondegenerate (VectorCurve2D.squaredMagnitude_ curve)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Nondegenerate (VectorCurve2D units1 space) ->
  Nondegenerate (Curve1D units2)
squaredMagnitude = Units.specialize . squaredMagnitude_

direction :: Nondegenerate (VectorCurve2D units space) -> DirectionCurve2D space
direction = DirectionCurve2D.unsafe . normalize
