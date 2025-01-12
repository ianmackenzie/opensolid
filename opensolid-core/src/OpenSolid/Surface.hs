module OpenSolid.Surface
  ( Surface
  , Function
  , parametric
  , function
  )
where

import OpenSolid.Region2d (Region2d)
import OpenSolid.Surface.Function (Function)
import OpenSolid.SurfaceParameter (UvCoordinates)

data Surface units where
  Parametric :: Function units -> Region2d UvCoordinates -> Surface units

parametric :: Function units -> Region2d UvCoordinates -> Surface units
parametric = Parametric

function :: Surface units -> Function units
function (Parametric f _) = f
