module OpenSolid.VectorSurface2d
  ( VectorSurface2d
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2d (Region2d)
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)

data VectorSurface2d units space where
  Parametric ::
    VectorSurfaceFunction2d units space ->
    Region2d Unitless UvSpace ->
    VectorSurface2d units space

parametric ::
  VectorSurfaceFunction2d units space ->
  Region2d Unitless UvSpace ->
  VectorSurface2d units space
parametric = Parametric

function :: VectorSurface2d units space -> VectorSurfaceFunction2d units space
function (Parametric f _) = f
