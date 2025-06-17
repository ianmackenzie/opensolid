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

type Function = VectorSurfaceFunction3d

data VectorSurface3d units where
  Parametric :: VectorSurfaceFunction3d units -> Region2d UvCoordinates -> VectorSurface3d units

parametric :: VectorSurfaceFunction3d units -> Region2d UvCoordinates -> VectorSurface3d units
parametric = Parametric

function :: VectorSurface3d units -> VectorSurfaceFunction3d units
function (Parametric f _) = f
