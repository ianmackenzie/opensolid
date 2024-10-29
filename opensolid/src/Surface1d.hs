module Surface1d
  ( Surface1d
  , Function
  , parametric
  , function
  )
where

import Region2d (Region2d)
import Surface1d.Function (Function)
import SurfaceParameter (UvCoordinates)

data Surface1d units where
  Parametric :: Function units -> Region2d UvCoordinates -> Surface1d units

parametric :: Function units -> Region2d UvCoordinates -> Surface1d units
parametric = Parametric

function :: Surface1d units -> Function units
function (Parametric f _) = f
