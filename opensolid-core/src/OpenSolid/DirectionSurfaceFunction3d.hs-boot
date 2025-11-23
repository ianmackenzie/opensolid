module OpenSolid.DirectionSurfaceFunction3d
  ( DirectionSurfaceFunction3d
  , unsafe
  , unwrap
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)

newtype DirectionSurfaceFunction3d space
  = DirectionSurfaceFunction3d (VectorSurfaceFunction3d space Unitless)

unsafe :: VectorSurfaceFunction3d space Unitless -> DirectionSurfaceFunction3d space
unwrap :: DirectionSurfaceFunction3d space -> VectorSurfaceFunction3d space Unitless
