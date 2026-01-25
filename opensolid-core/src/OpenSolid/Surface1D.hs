module OpenSolid.Surface1D
  ( Surface1D
  , parametric
  , function
  )
where

import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.UvRegion (UvRegion)

data Surface1D units where
  Parametric :: SurfaceFunction1D units -> UvRegion -> Surface1D units

parametric :: SurfaceFunction1D units -> UvRegion -> Surface1D units
parametric = Parametric

function :: Surface1D units -> SurfaceFunction1D units
function (Parametric f _) = f
