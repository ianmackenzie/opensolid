module OpenSolid.VectorSurface2d
  ( VectorSurface2d
  , parametric
  , function
  )
where

import OpenSolid.Region2d (Region2d)
import OpenSolid.SurfaceParameter (UvCoordinates)
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)

data VectorSurface2d units where
  Parametric :: VectorSurfaceFunction2d units -> Region2d UvCoordinates -> VectorSurface2d units

parametric :: VectorSurfaceFunction2d units -> Region2d UvCoordinates -> VectorSurface2d units
parametric = Parametric

function :: VectorSurface2d units -> VectorSurfaceFunction2d units
function (Parametric f _) = f
