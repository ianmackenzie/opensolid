module VectorSurface3d
  ( VectorSurface3d
  , Function
  , parametric
  , function
  )
where

import Region2d (Region2d)
import Uv qualified
import VectorSurface3d.Function (Function)

data VectorSurface3d units where
  Parametric :: Function units -> Region2d Uv.Coordinates -> VectorSurface3d units

parametric :: Function units -> Region2d Uv.Coordinates -> VectorSurface3d units
parametric = Parametric

function :: VectorSurface3d units -> Function units
function (Parametric f _) = f
