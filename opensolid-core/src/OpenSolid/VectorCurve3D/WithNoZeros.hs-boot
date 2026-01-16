module OpenSolid.VectorCurve3D.WithNoZeros
  ( WithNoZeros (WithNoZeros)
  , unwrap
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  )
where

import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)

newtype WithNoZeros units space = WithNoZeros (VectorCurve3D units space)

unwrap :: WithNoZeros units space -> VectorCurve3D units space
squaredMagnitude_ :: WithNoZeros units space -> Curve1D.WithNoZeros (units ?*? units)
squaredMagnitude ::
  Units.Squared units1 units2 =>
  WithNoZeros units1 space ->
  Curve1D.WithNoZeros units2
magnitude :: WithNoZeros units space -> Curve1D.WithNoZeros units
