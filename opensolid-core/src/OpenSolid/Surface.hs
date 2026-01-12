module OpenSolid.Surface
  ( Surface
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2D (Region2D)
import OpenSolid.SurfaceFunction (SurfaceFunction)

data Surface units where
  Parametric :: SurfaceFunction units -> Region2D Unitless UvSpace -> Surface units

parametric :: SurfaceFunction units -> Region2D Unitless UvSpace -> Surface units
parametric = Parametric

function :: Surface units -> SurfaceFunction units
function (Parametric f _) = f
