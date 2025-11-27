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

type Function units space = VectorSurfaceFunction3d units space

data VectorSurface3d units space where
  Parametric ::
    VectorSurfaceFunction3d units space ->
    Region2d Unitless UvSpace ->
    VectorSurface3d units space

parametric ::
  VectorSurfaceFunction3d units space ->
  Region2d Unitless UvSpace ->
  VectorSurface3d units space
parametric = Parametric

function :: VectorSurface3d units space -> VectorSurfaceFunction3d units space
function (Parametric f _) = f
