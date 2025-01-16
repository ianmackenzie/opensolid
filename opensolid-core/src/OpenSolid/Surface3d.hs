module OpenSolid.Surface3d
  ( Surface3d
  , parametric
  , function
  , domain
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2d (Region2d)
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceParameter (UvCoordinates)

data Surface3d (coordinateSystem :: CoordinateSystem) where
  Surface3d ::
    SurfaceFunction3d (space @ units) -> Region2d UvCoordinates -> Surface3d (space @ units)

parametric ::
  SurfaceFunction3d (space @ units) ->
  Region2d UvCoordinates ->
  Surface3d (space @ units)
parametric = Surface3d

function :: Surface3d units -> SurfaceFunction3d units
function (Surface3d f _) = f

domain :: Surface3d units -> Region2d UvCoordinates
domain (Surface3d _ d) = d
