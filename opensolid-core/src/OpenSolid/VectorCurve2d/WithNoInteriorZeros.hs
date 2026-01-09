module OpenSolid.VectorCurve2d.WithNoInteriorZeros
  ( WithNoInteriorZeros (WithNoInteriorZeros)
  , unwrap
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  )
where

import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.WithNoInteriorZeros qualified as Curve.WithNoInteriorZeros
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

newtype WithNoInteriorZeros units space = WithNoInteriorZeros (VectorCurve2d units space)

instance HasUnits (WithNoInteriorZeros units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (WithNoInteriorZeros units1 space1) (WithNoInteriorZeros units2 space2)
  where
  coerce (WithNoInteriorZeros curve) = WithNoInteriorZeros (Units.coerce curve)

unwrap :: WithNoInteriorZeros units space -> VectorCurve2d units space
unwrap (WithNoInteriorZeros curve) = curve

squaredMagnitude_ :: WithNoInteriorZeros units space -> Curve.WithNoInteriorZeros (units ?*? units)
squaredMagnitude_ (WithNoInteriorZeros curve) =
  Curve.WithNoInteriorZeros (VectorCurve2d.squaredMagnitude_ curve)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  WithNoInteriorZeros units1 space ->
  Curve.WithNoInteriorZeros units2
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude :: WithNoInteriorZeros units space -> Curve.WithNoInteriorZeros units
magnitude withNoInteriorZeros =
  Curve.WithNoInteriorZeros.sqrt_ (squaredMagnitude_ withNoInteriorZeros)
