module OpenSolid.VectorCurve.Nondegenerate
  ( Nondegenerate
  , unsafe
  , magnitude
  , normalized
  , direction
  )
where

import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.WithNoInteriorZeros qualified as Curve1D.WithNoInteriorZeros
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Prelude
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve

data Nondegenerate dimension units space = Nondegenerate
  { vectorCurve :: VectorCurve dimension units space
  , magnitude :: ~(Curve1D.WithNoInteriorZeros units)
  , normalized :: ~(VectorCurve dimension Unitless space)
  }

unsafe ::
  VectorCurve.Exists dimension units space =>
  VectorCurve dimension units space -> Nondegenerate dimension units space
unsafe vectorCurve = do
  let curveSquaredMagnitude_ =
        Curve1D.WithNoInteriorZeros (VectorCurve.squaredMagnitude_ vectorCurve)
  let curveMagnitude = Curve1D.WithNoInteriorZeros.sqrt_ curveSquaredMagnitude_
  Nondegenerate
    { vectorCurve
    , magnitude = curveMagnitude
    , normalized = vectorCurve / curveMagnitude
    }

magnitude :: Nondegenerate dimension units space -> Curve1D.WithNoInteriorZeros units
magnitude = (.magnitude)

normalized :: Nondegenerate dimension units space -> VectorCurve dimension Unitless space
normalized = (.normalized)

direction ::
  DirectionCurve.Exists dimension space =>
  Nondegenerate dimension units space ->
  DirectionCurve dimension space
direction nondegenerate = DirectionCurve.unsafe nondegenerate.normalized
