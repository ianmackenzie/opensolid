module OpenSolid.VectorCurve2D.Nondegenerate
  ( Nondegenerate
  , unsafe
  , curve
  , magnitude
  , normalize
  , direction
  , squaredMagnitude_
  , squaredMagnitude
  )
where

import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
import {-# SOURCE #-} OpenSolid.Curve1D.Nondegenerate qualified as Curve1D.Nondegenerate
import {-# SOURCE #-} OpenSolid.DirectionCurve2D (DirectionCurve2D)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)
import {-# SOURCE #-} OpenSolid.VectorCurve2D qualified as VectorCurve2D

data Nondegenerate units space = Nondegenerate
  { curve :: VectorCurve2D units space
  , magnitude :: ~(Curve1D.Nondegenerate units)
  , normalized :: ~(VectorCurve2D Unitless space)
  }

instance HasUnits (Nondegenerate units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (Nondegenerate units1 space1) (Nondegenerate units2 space2)
  where
  coerce nondegenerate =
    Nondegenerate
      { curve = Units.coerce nondegenerate.curve
      , magnitude = Units.coerce nondegenerate.magnitude
      , normalized = nondegenerate.normalized
      }

unsafe :: VectorCurve2D units space -> Nondegenerate units space
unsafe givenCurve = do
  let curveSquaredMagnitude_ = Curve1D.Nondegenerate (VectorCurve2D.squaredMagnitude_ givenCurve)
  let curveMagnitude = Curve1D.Nondegenerate.sqrt_ curveSquaredMagnitude_
  Nondegenerate
    { curve = givenCurve
    , magnitude = curveMagnitude
    , normalized = givenCurve / curveMagnitude
    }

curve :: Nondegenerate units space -> VectorCurve2D units space
curve = (.curve)

magnitude :: Nondegenerate units space -> Curve1D.Nondegenerate units
magnitude = (.magnitude)

normalize :: Nondegenerate units space -> VectorCurve2D Unitless space
normalize = (.normalized)

squaredMagnitude_ :: Nondegenerate units space -> Curve1D.Nondegenerate (units ?*? units)
squaredMagnitude_ nondegenerate =
  Curve1D.Nondegenerate (VectorCurve2D.squaredMagnitude_ nondegenerate.curve)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Nondegenerate units1 space ->
  Curve1D.Nondegenerate units2
squaredMagnitude = Units.specialize . squaredMagnitude_

direction :: Nondegenerate units space -> DirectionCurve2D space
direction nondegenerate = DirectionCurve2D.unsafe nondegenerate.normalized
