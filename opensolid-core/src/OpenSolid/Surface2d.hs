module OpenSolid.Surface2d
  ( Surface2d
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2d (Region2d)
import OpenSolid.SurfaceFunction2d (SurfaceFunction2d)

data Surface2d units space where
  Parametric :: SurfaceFunction2d units space -> Region2d Unitless UvSpace -> Surface2d units space

parametric :: SurfaceFunction2d units space -> Region2d Unitless UvSpace -> Surface2d units space
parametric = Parametric

function :: Surface2d units space -> SurfaceFunction2d units space
function (Parametric f _) = f
