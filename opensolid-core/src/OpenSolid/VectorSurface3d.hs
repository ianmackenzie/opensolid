module OpenSolid.VectorSurface3d
  ( VectorSurface3d
  , Function
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2d (Region2d)
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)

type Function space units = VectorSurfaceFunction3d space units

data VectorSurface3d space units where
  Parametric ::
    VectorSurfaceFunction3d space units ->
    Region2d UvSpace Unitless ->
    VectorSurface3d space units

parametric ::
  VectorSurfaceFunction3d space units ->
  Region2d UvSpace Unitless ->
  VectorSurface3d space units
parametric = Parametric

function :: VectorSurface3d space units -> VectorSurfaceFunction3d space units
function (Parametric f _) = f
