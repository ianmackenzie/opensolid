module OpenSolid.VectorCurve2d.WithNoZeros
  ( WithNoZeros (WithNoZeros)
  , unwrap
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  )
where

import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.WithNoZeros qualified as Curve.WithNoZeros
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

newtype WithNoZeros units space = WithNoZeros (VectorCurve2d units space)

instance HasUnits (WithNoZeros units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (WithNoZeros units1 space1) (WithNoZeros units2 space2)
  where
  coerce (WithNoZeros curve) = WithNoZeros (Units.coerce curve)

unwrap :: WithNoZeros units space -> VectorCurve2d units space
unwrap (WithNoZeros curve) = curve

squaredMagnitude_ :: WithNoZeros units space -> Curve.WithNoZeros (units ?*? units)
squaredMagnitude_ (WithNoZeros curve) = Curve.WithNoZeros (VectorCurve2d.squaredMagnitude_ curve)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  WithNoZeros units1 space ->
  Curve.WithNoZeros units2
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude :: WithNoZeros units space -> Curve.WithNoZeros units
magnitude withNoZeros = Curve.WithNoZeros.sqrt_ (squaredMagnitude_ withNoZeros)
