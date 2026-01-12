module OpenSolid.Surface2D
  ( Surface2D
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2D (Region2D)
import OpenSolid.SurfaceFunction2D (SurfaceFunction2D)

data Surface2D units space where
  Parametric :: SurfaceFunction2D units space -> Region2D Unitless UvSpace -> Surface2D units space

parametric :: SurfaceFunction2D units space -> Region2D Unitless UvSpace -> Surface2D units space
parametric = Parametric

function :: Surface2D units space -> SurfaceFunction2D units space
function (Parametric f _) = f
