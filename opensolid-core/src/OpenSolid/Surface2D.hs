module OpenSolid.Surface2D
  ( Surface2D
  , parametric
  , function
  )
where

import OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import OpenSolid.UvRegion (UvRegion)

data Surface2D units where
  Parametric :: SurfaceFunction2D units -> UvRegion -> Surface2D units

parametric :: SurfaceFunction2D units -> UvRegion -> Surface2D units
parametric = Parametric

function :: Surface2D units -> SurfaceFunction2D units
function (Parametric f _) = f
