module OpenSolid.DirectionSurfaceFunction2D
  ( DirectionSurfaceFunction2D
  , unsafe
  , unwrap
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)

newtype DirectionSurfaceFunction2D
  = DirectionSurfaceFunction2D (VectorSurfaceFunction2D Unitless)

unsafe :: VectorSurfaceFunction2D Unitless -> DirectionSurfaceFunction2D
unwrap :: DirectionSurfaceFunction2D -> VectorSurfaceFunction2D Unitless
