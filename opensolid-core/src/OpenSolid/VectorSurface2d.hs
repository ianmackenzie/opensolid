module OpenSolid.VectorSurface2d
  ( VectorSurface2d
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2d (Region2d)
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)

data VectorSurface2d space units where
  Parametric ::
    VectorSurfaceFunction2d space units ->
    Region2d UvSpace Unitless ->
    VectorSurface2d space units

parametric ::
  VectorSurfaceFunction2d space units ->
  Region2d UvSpace Unitless ->
  VectorSurface2d space units
parametric = Parametric

function :: VectorSurface2d space units -> VectorSurfaceFunction2d space units
function (Parametric f _) = f
