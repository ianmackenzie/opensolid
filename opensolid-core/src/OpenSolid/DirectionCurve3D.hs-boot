module OpenSolid.DirectionCurve3D
  ( DirectionCurve3D
  , unsafe
  , unwrap
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)

newtype DirectionCurve3D space = DirectionCurve3D (VectorCurve3D Unitless space)

unsafe :: VectorCurve3D Unitless space -> DirectionCurve3D space
unwrap :: DirectionCurve3D space -> VectorCurve3D Unitless space
evaluate :: DirectionCurve3D space -> Number -> Direction3D space
evaluateBounds :: DirectionCurve3D space -> Interval Unitless -> DirectionBounds3D space
derivative :: DirectionCurve3D space -> VectorCurve3D Unitless space
