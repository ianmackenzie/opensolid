module OpenSolid.VectorCurve3D.WithNoInteriorZeros
  ( WithNoInteriorZeros (WithNoInteriorZeros)
  , unwrap
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  )
where

import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.WithNoInteriorZeros qualified as Curve1D.WithNoInteriorZeros
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D

newtype WithNoInteriorZeros units space = WithNoInteriorZeros (VectorCurve3D units space)

instance HasUnits (WithNoInteriorZeros units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (WithNoInteriorZeros units1 space1) (WithNoInteriorZeros units2 space2)
  where
  coerce (WithNoInteriorZeros curve) = WithNoInteriorZeros (Units.coerce curve)

unwrap :: WithNoInteriorZeros units space -> VectorCurve3D units space
unwrap (WithNoInteriorZeros curve) = curve

squaredMagnitude_ :: WithNoInteriorZeros units space -> Curve1D.WithNoInteriorZeros (units ?*? units)
squaredMagnitude_ (WithNoInteriorZeros curve) =
  Curve1D.WithNoInteriorZeros (VectorCurve3D.squaredMagnitude_ curve)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  WithNoInteriorZeros units1 space ->
  Curve1D.WithNoInteriorZeros units2
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude :: WithNoInteriorZeros units space -> Curve1D.WithNoInteriorZeros units
magnitude withNoInteriorZeros =
  Curve1D.WithNoInteriorZeros.sqrt_ (squaredMagnitude_ withNoInteriorZeros)
