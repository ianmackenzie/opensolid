module OpenSolid.VectorCurve2d.WithNoZeros
  ( WithNoZeros (WithNoZeros)
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

newtype WithNoZeros units space = WithNoZeros (VectorCurve2d units space)

unwrap :: WithNoZeros units space -> VectorCurve2d units space
squaredMagnitude_ :: WithNoZeros units space -> Curve.WithNoZeros (units ?*? units)
squaredMagnitude ::
  Units.Squared units1 units2 =>
  WithNoZeros units1 space ->
  Curve.WithNoZeros units2
magnitude :: WithNoZeros units space -> Curve.WithNoZeros units
