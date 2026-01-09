module OpenSolid.VectorCurve2d.WithNoInteriorZeros
  ( WithNoInteriorZeros (WithNoInteriorZeros)
  , unwrap
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  )
where

import OpenSolid.Curve qualified as Curve
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)

newtype WithNoInteriorZeros units space = WithNoInteriorZeros (VectorCurve2d units space)

unwrap :: WithNoInteriorZeros units space -> VectorCurve2d units space
squaredMagnitude_ :: WithNoInteriorZeros units space -> Curve.WithNoInteriorZeros (units ?*? units)
squaredMagnitude ::
  Units.Squared units1 units2 =>
  WithNoInteriorZeros units1 space ->
  Curve.WithNoInteriorZeros units2
magnitude :: WithNoInteriorZeros units space -> Curve.WithNoInteriorZeros units
