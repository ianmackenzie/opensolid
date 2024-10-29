module VectorSurface2d
  ( VectorSurface2d
  , Function
  , parametric
  , function
  )
where

import Region2d (Region2d)
import SurfaceParameter (UvCoordinates)
import VectorSurface2d.Function (Function)

data VectorSurface2d units where
  Parametric :: Function units -> Region2d UvCoordinates -> VectorSurface2d units

parametric :: Function units -> Region2d UvCoordinates -> VectorSurface2d units
parametric = Parametric

function :: VectorSurface2d units -> Function units
function (Parametric f _) = f
