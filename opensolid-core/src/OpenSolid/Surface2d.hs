module OpenSolid.Surface2d
  ( Surface2d
  , parametric
  , function
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2d (Region2d)
import OpenSolid.SurfaceFunction2d (SurfaceFunction2d)

data Surface2d space units where
  Parametric :: SurfaceFunction2d space units -> Region2d UvSpace Unitless -> Surface2d space units

parametric :: SurfaceFunction2d space units -> Region2d UvSpace Unitless -> Surface2d space units
parametric = Parametric

function :: Surface2d space units -> SurfaceFunction2d space units
function (Parametric f _) = f
