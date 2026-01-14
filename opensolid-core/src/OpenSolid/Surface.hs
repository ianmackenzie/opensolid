module OpenSolid.Surface
  ( Surface
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2D (Region2D)
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)

data Surface units where
  Parametric :: SurfaceFunction1D units -> Region2D Unitless UvSpace -> Surface units

parametric :: SurfaceFunction1D units -> Region2D Unitless UvSpace -> Surface units
parametric = Parametric

function :: Surface units -> SurfaceFunction1D units
function (Parametric f _) = f
