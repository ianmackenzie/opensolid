module OpenSolid.VectorSurface3d
  ( VectorSurface3d
  , Function
  , parametric
  , function
  )
where

import OpenSolid.Region2d (Region2d)
import OpenSolid.VectorSurface3d.Function (Function)
import OpenSolid.SurfaceParameter (UvCoordinates)

data VectorSurface3d units where
  Parametric :: Function units -> Region2d UvCoordinates -> VectorSurface3d units

parametric :: Function units -> Region2d UvCoordinates -> VectorSurface3d units
parametric = Parametric

function :: VectorSurface3d units -> Function units
function (Parametric f _) = f
