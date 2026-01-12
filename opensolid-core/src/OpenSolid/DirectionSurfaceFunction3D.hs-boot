module OpenSolid.DirectionSurfaceFunction3D
  ( DirectionSurfaceFunction3D
  , unsafe
  , unwrap
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)

newtype DirectionSurfaceFunction3D space
  = DirectionSurfaceFunction3D (VectorSurfaceFunction3D Unitless space)

unsafe :: VectorSurfaceFunction3D Unitless space -> DirectionSurfaceFunction3D space
unwrap :: DirectionSurfaceFunction3D space -> VectorSurfaceFunction3D Unitless space
