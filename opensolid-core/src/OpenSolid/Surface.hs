module OpenSolid.Surface
  ( Surface
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2d (Region2d)
import OpenSolid.SurfaceFunction (SurfaceFunction)

data Surface units where
  Parametric :: SurfaceFunction units -> Region2d UvSpace Unitless -> Surface units

parametric :: SurfaceFunction units -> Region2d UvSpace Unitless -> Surface units
parametric = Parametric

function :: Surface units -> SurfaceFunction units
function (Parametric f _) = f
