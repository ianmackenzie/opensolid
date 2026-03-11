module OpenSolid.VectorCurve3D.Nondegenerate
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

import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Nondegenerate qualified as Curve1D.Nondegenerate
import {-# SOURCE #-} OpenSolid.DirectionCurve3D (DirectionCurve3D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D qualified as DirectionCurve3D
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D qualified as VectorCurve3D

data Nondegenerate units space = Nondegenerate
  { curve :: VectorCurve3D units space
  , magnitude :: ~(Curve1D.Nondegenerate units)
  , normalized :: ~(VectorCurve3D Unitless space)
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

unsafe :: VectorCurve3D units space -> Nondegenerate units space
unsafe givenCurve = do
  let curveSquaredMagnitude_ = Curve1D.Nondegenerate (VectorCurve3D.squaredMagnitude_ givenCurve)
  let curveMagnitude = Curve1D.Nondegenerate.sqrt_ curveSquaredMagnitude_
  Nondegenerate
    { curve = givenCurve
    , magnitude = curveMagnitude
    , normalized = givenCurve / curveMagnitude
    }

curve :: Nondegenerate units space -> VectorCurve3D units space
curve = (.curve)

magnitude :: Nondegenerate units space -> Curve1D.Nondegenerate units
magnitude = (.magnitude)

normalize :: Nondegenerate units space -> VectorCurve3D Unitless space
normalize = (.normalized)

squaredMagnitude_ :: Nondegenerate units space -> Curve1D.Nondegenerate (units ?*? units)
squaredMagnitude_ nondegenerate =
  Curve1D.Nondegenerate (VectorCurve3D.squaredMagnitude_ nondegenerate.curve)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Nondegenerate units1 space ->
  Curve1D.Nondegenerate units2
squaredMagnitude = Units.specialize . squaredMagnitude_

direction :: Nondegenerate units space -> DirectionCurve3D space
direction nondegenerate = DirectionCurve3D.unsafe nondegenerate.normalized
