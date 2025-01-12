module OpenSolid.Surface2d
  ( Surface2d
  , parametric
  , function
  )
where

import OpenSolid.Region2d (Region2d)
import OpenSolid.SurfaceFunction2d (SurfaceFunction2d)
import OpenSolid.SurfaceParameter (UvCoordinates)

data Surface2d units where
  Parametric :: SurfaceFunction2d units -> Region2d UvCoordinates -> Surface2d units

parametric :: SurfaceFunction2d units -> Region2d UvCoordinates -> Surface2d units
parametric = Parametric

function :: Surface2d units -> SurfaceFunction2d units
function (Parametric f _) = f
