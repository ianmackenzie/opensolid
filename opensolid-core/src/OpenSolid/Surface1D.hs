module OpenSolid.Surface1D
  ( Surface1D
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2D (Region2D)
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)

data Surface1D units where
  Parametric :: SurfaceFunction1D units -> Region2D Unitless UvSpace -> Surface1D units

parametric :: SurfaceFunction1D units -> Region2D Unitless UvSpace -> Surface1D units
parametric = Parametric

function :: Surface1D units -> SurfaceFunction1D units
function (Parametric f _) = f
