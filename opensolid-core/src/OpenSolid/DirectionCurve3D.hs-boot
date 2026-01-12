module OpenSolid.DirectionCurve3D
  ( DirectionCurve3D
  , unsafe
  , unwrap
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)

newtype DirectionCurve3D space = DirectionCurve3D (VectorCurve3D Unitless space)

unsafe :: VectorCurve3D Unitless space -> DirectionCurve3D space
unwrap :: DirectionCurve3D space -> VectorCurve3D Unitless space
