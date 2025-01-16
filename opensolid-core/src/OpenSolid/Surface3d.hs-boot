module OpenSolid.Surface3d
  ( Surface3d
  , parametric
  )
where

import OpenSolid.Prelude
import OpenSolid.Region2d (Region2d)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceParameter (UvCoordinates)

type role Surface3d nominal

data Surface3d (coordinateSystem :: CoordinateSystem)

parametric ::
  SurfaceFunction3d (space @ units) ->
  Region2d UvCoordinates ->
  Surface3d (space @ units)
