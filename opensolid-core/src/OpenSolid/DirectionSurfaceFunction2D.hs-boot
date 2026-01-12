module OpenSolid.DirectionSurfaceFunction2D
  ( DirectionSurfaceFunction2D
  , unsafe
  , unwrap
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)

newtype DirectionSurfaceFunction2D space
  = DirectionSurfaceFunction2D (VectorSurfaceFunction2D Unitless space)

unsafe :: VectorSurfaceFunction2D Unitless space -> DirectionSurfaceFunction2D space
unwrap :: DirectionSurfaceFunction2D space -> VectorSurfaceFunction2D Unitless space
