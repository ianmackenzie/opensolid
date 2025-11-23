module OpenSolid.DirectionSurfaceFunction2d
  ( DirectionSurfaceFunction2d
  , unsafe
  , unwrap
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)

newtype DirectionSurfaceFunction2d space
  = DirectionSurfaceFunction2d (VectorSurfaceFunction2d space Unitless)

unsafe :: VectorSurfaceFunction2d space Unitless -> DirectionSurfaceFunction2d space
unwrap :: DirectionSurfaceFunction2d space -> VectorSurfaceFunction2d space Unitless
