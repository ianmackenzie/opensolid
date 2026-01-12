module OpenSolid.VectorSurface3D
  ( VectorSurface3D
  , Function
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2D (Region2D)
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)

type Function units space = VectorSurfaceFunction3D units space

data VectorSurface3D units space where
  Parametric ::
    VectorSurfaceFunction3D units space ->
    Region2D Unitless UvSpace ->
    VectorSurface3D units space

parametric ::
  VectorSurfaceFunction3D units space ->
  Region2D Unitless UvSpace ->
  VectorSurface3D units space
parametric = Parametric

function :: VectorSurface3D units space -> VectorSurfaceFunction3D units space
function (Parametric f _) = f
