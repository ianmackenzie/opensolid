module OpenSolid.Surface
  ( Surface
  , parametric
  , function
  )
where

import OpenSolid.Region2d (Region2d)
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (UvCoordinates)

data Surface units where
  Parametric :: SurfaceFunction units -> Region2d UvCoordinates -> Surface units

parametric :: SurfaceFunction units -> Region2d UvCoordinates -> Surface units
parametric = Parametric

function :: Surface units -> SurfaceFunction units
function (Parametric f _) = f
