module OpenSolid.Surface3D
  ( Surface3D
  , parametric
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2D (Region2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)

type role Surface3D nominal

type Surface3D :: Type -> Type
data Surface3D space

parametric :: SurfaceFunction3D space -> Region2D Unitless UvSpace -> Surface3D space
