module OpenSolid.VectorCurve3D.WithNoZeros
  ( WithNoZeros (WithNoZeros)
  , unwrap
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  )
where

import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.WithNoZeros qualified as Curve1D.WithNoZeros
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D

newtype WithNoZeros units space = WithNoZeros (VectorCurve3D units space)

instance HasUnits (WithNoZeros units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (WithNoZeros units1 space1) (WithNoZeros units2 space2)
  where
  coerce (WithNoZeros curve) = WithNoZeros (Units.coerce curve)

unwrap :: WithNoZeros units space -> VectorCurve3D units space
unwrap (WithNoZeros curve) = curve

squaredMagnitude_ :: WithNoZeros units space -> Curve1D.WithNoZeros (units ?*? units)
squaredMagnitude_ (WithNoZeros curve) = Curve1D.WithNoZeros (VectorCurve3D.squaredMagnitude_ curve)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  WithNoZeros units1 space ->
  Curve1D.WithNoZeros units2
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude :: WithNoZeros units space -> Curve1D.WithNoZeros units
magnitude withNoZeros = Curve1D.WithNoZeros.sqrt_ (squaredMagnitude_ withNoZeros)
