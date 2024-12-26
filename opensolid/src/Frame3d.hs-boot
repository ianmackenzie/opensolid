module Frame3d
  ( Frame3d
  , originPoint
  , basis
  , xDirection
  , yDirection
  , zDirection
  )
where

import {-# SOURCE #-} Basis3d (Basis3d)
import {-# SOURCE #-} Direction3d (Direction3d)
import {-# SOURCE #-} OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude

type role Frame3d nominal nominal

type Frame3d :: CoordinateSystem -> LocalSpace -> Type
data Frame3d coordinateSystem defines where
  Frame3d ::
    { originPoint :: Point3d (space @ units)
    , basis :: Basis3d space defines
    } ->
    Frame3d (space @ units) defines

xDirection :: Frame3d (space @ units) defines -> Direction3d space
yDirection :: Frame3d (space @ units) defines -> Direction3d space
zDirection :: Frame3d (space @ units) defines -> Direction3d space
