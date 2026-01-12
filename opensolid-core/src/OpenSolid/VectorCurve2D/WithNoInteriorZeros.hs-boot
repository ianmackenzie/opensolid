module OpenSolid.VectorCurve2D.WithNoInteriorZeros
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
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

newtype WithNoInteriorZeros units space = WithNoInteriorZeros (VectorCurve2D units space)

unwrap :: WithNoInteriorZeros units space -> VectorCurve2D units space
squaredMagnitude_ :: WithNoInteriorZeros units space -> Curve.WithNoInteriorZeros (units ?*? units)
squaredMagnitude ::
  Units.Squared units1 units2 =>
  WithNoInteriorZeros units1 space ->
  Curve.WithNoInteriorZeros units2
magnitude :: WithNoInteriorZeros units space -> Curve.WithNoInteriorZeros units
