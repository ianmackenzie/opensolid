module OpenSolid.Surface3D
  ( Surface3D
  , parametric
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.UvRegion (UvRegion)

type role Surface3D nominal

type Surface3D :: Type -> Type
data Surface3D space

parametric :: SurfaceFunction3D space -> UvRegion -> Surface3D space
