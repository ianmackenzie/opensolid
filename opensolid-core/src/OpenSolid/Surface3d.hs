module OpenSolid.Surface3d
  ( Surface3d
  , new
  , function
  )
where

import OpenSolid.Region2d (Region2d)
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceParameter (UvCoordinates)

data Surface3d units where
  Surface3d :: SurfaceFunction3d units -> Region2d UvCoordinates -> Surface3d units

new :: SurfaceFunction3d units -> Region2d UvCoordinates -> Surface3d units
new = Surface3d

function :: Surface3d units -> SurfaceFunction3d units
function (Surface3d f _) = f
