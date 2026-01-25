module OpenSolid.Surface2D
  ( Surface2D
  , parametric
  , function
  )
where

import OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import OpenSolid.UvRegion (UvRegion)

data Surface2D units space where
  Parametric :: SurfaceFunction2D units space -> UvRegion -> Surface2D units space

parametric :: SurfaceFunction2D units space -> UvRegion -> Surface2D units space
parametric = Parametric

function :: Surface2D units space -> SurfaceFunction2D units space
function (Parametric f _) = f
