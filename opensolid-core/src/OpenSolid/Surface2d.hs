module OpenSolid.Surface2d
  ( Surface2d
  , Function
  , parametric
  , function
  )
where

import OpenSolid.Region2d (Region2d)
import OpenSolid.Surface2d.Function (Function)
import OpenSolid.SurfaceParameter (UvCoordinates)

data Surface2d units where
  Parametric :: Function units -> Region2d UvCoordinates -> Surface2d units

parametric :: Function units -> Region2d UvCoordinates -> Surface2d units
parametric = Parametric

function :: Surface2d units -> Function units
function (Parametric f _) = f
