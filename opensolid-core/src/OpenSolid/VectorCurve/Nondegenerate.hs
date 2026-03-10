module OpenSolid.VectorCurve.Nondegenerate
  ( Nondegenerate
  , unsafe
  , curve
  , magnitude
  , normalized
  , direction
  )
where

import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Nondegenerate qualified as Curve1D.Nondegenerate
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve

data Nondegenerate dimension units space = Nondegenerate
  { curve :: VectorCurve dimension units space
  , magnitude :: ~(Curve1D.Nondegenerate units)
  , normalized :: ~(VectorCurve dimension Unitless space)
  }

instance HasUnits (Nondegenerate dimension units space) units

instance
  ( VectorCurve.Exists dimension1 units1 space1
  , VectorCurve.Exists dimension2 units2 space2
  , dimension1 ~ dimension2
  , space1 ~ space2
  ) =>
  Units.Coercion
    (Nondegenerate dimension1 units1 space1)
    (Nondegenerate dimension2 units2 space2)
  where
  coerce nondegenerate =
    Nondegenerate
      { curve = VectorCurve.coerce nondegenerate.curve
      , magnitude = Units.coerce nondegenerate.magnitude
      , normalized = nondegenerate.normalized
      }

unsafe ::
  VectorCurve.Exists dimension units space =>
  VectorCurve dimension units space -> Nondegenerate dimension units space
unsafe givenCurve = do
  let curveSquaredMagnitude_ = Curve1D.Nondegenerate (VectorCurve.squaredMagnitude_ givenCurve)
  let curveMagnitude = Curve1D.Nondegenerate.sqrt_ curveSquaredMagnitude_
  Nondegenerate
    { curve = givenCurve
    , magnitude = curveMagnitude
    , normalized = givenCurve / curveMagnitude
    }

curve :: Nondegenerate dimension units space -> VectorCurve dimension units space
curve = (.curve)

magnitude :: Nondegenerate dimension units space -> Curve1D.Nondegenerate units
magnitude = (.magnitude)

normalized :: Nondegenerate dimension units space -> VectorCurve dimension Unitless space
normalized = (.normalized)

direction ::
  DirectionCurve.Exists dimension space =>
  Nondegenerate dimension units space ->
  DirectionCurve dimension space
direction nondegenerate = DirectionCurve.unsafe nondegenerate.normalized
