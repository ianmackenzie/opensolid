module OpenSolid.Surface3d
  ( Surface3d
  , Function
  , new
  , function
  )
where

import OpenSolid.Region2d (Region2d)
import OpenSolid.Surface3d.Function (Function)
import OpenSolid.SurfaceParameter (UvCoordinates)

data Surface3d units where
  Surface3d :: Function units -> Region2d UvCoordinates -> Surface3d units

new :: Function units -> Region2d UvCoordinates -> Surface3d units
new = Surface3d

function :: Surface3d units -> Function units
function (Surface3d f _) = f
