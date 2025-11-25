module OpenSolid.Surface3d
  ( Surface3d
  , parametric
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2d (Region2d)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d (SurfaceFunction3d)

type role Surface3d nominal

type Surface3d :: Type -> Type
data Surface3d space

parametric ::
  SurfaceFunction3d space Meters ->
  Region2d UvSpace Unitless ->
  Surface3d space
