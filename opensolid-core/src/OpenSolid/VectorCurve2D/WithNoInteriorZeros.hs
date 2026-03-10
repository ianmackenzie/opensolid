module OpenSolid.VectorCurve2D.WithNoInteriorZeros
  ( WithNoInteriorZeros (WithNoInteriorZeros)
  , unwrap
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  )
where

import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Nondegenerate qualified as Curve1D.Nondegenerate
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

newtype WithNoInteriorZeros units space = WithNoInteriorZeros (VectorCurve2D units space)

instance HasUnits (WithNoInteriorZeros units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (WithNoInteriorZeros units1 space1) (WithNoInteriorZeros units2 space2)
  where
  coerce (WithNoInteriorZeros curve) = WithNoInteriorZeros (Units.coerce curve)

unwrap :: WithNoInteriorZeros units space -> VectorCurve2D units space
unwrap (WithNoInteriorZeros curve) = curve

squaredMagnitude_ :: WithNoInteriorZeros units space -> Curve1D.Nondegenerate (units ?*? units)
squaredMagnitude_ (WithNoInteriorZeros curve) =
  Curve1D.Nondegenerate (VectorCurve2D.squaredMagnitude_ curve)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  WithNoInteriorZeros units1 space ->
  Curve1D.Nondegenerate units2
squaredMagnitude = Units.specialize . squaredMagnitude_

magnitude :: WithNoInteriorZeros units space -> Curve1D.Nondegenerate units
magnitude withNoInteriorZeros =
  Curve1D.Nondegenerate.sqrt_ (squaredMagnitude_ withNoInteriorZeros)
