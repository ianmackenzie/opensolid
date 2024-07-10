module Frame3d
  ( Frame3d
  , xyz
  , at
  , originPoint
  , basis
  , xDirection
  , yDirection
  , zDirection
  , xAxis
  , yAxis
  , zAxis
  , placeIn
  , relativeTo
  , inverse
  )
where

import Axis3d (Axis3d)
import Axis3d qualified
import Basis3d (Basis3d)
import Basis3d qualified
import Direction3d (Direction3d)
import OpenSolid
import Point3d (Point3d)
import Point3d qualified

type role Frame3d nominal nominal

type Frame3d :: CoordinateSystem -> LocalSpace -> Type
data Frame3d coordinateSystem defines where
  Frame3d ::
    { originPoint :: Point3d (space @ units)
    , basis :: Basis3d space defines
    } ->
    Frame3d (space @ units) defines

deriving instance Show (Frame3d coordinateSystem defines)

xDirection :: Frame3d (space @ units) defines -> Direction3d space
xDirection frame = Basis3d.xDirection (basis frame)

yDirection :: Frame3d (space @ units) defines -> Direction3d space
yDirection frame = Basis3d.yDirection (basis frame)

zDirection :: Frame3d (space @ units) defines -> Direction3d space
zDirection frame = Basis3d.zDirection (basis frame)

xyz :: Frame3d (space @ units) defines
xyz = Frame3d Point3d.origin Basis3d.xyz

at :: Point3d (space @ units) -> Basis3d space defines -> Frame3d (space @ units) defines
at point basis = Frame3d point basis

xAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
xAxis frame = Axis3d.through (originPoint frame) (xDirection frame)

yAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
yAxis frame = Axis3d.through (originPoint frame) (yDirection frame)

zAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
zAxis frame = Axis3d.through (originPoint frame) (zDirection frame)

placeIn ::
  Frame3d (global @ units) (Defines space) ->
  Frame3d (space @ units) (Defines local) ->
  Frame3d (global @ units) (Defines local)
placeIn globalFrame frame =
  Frame3d
    { originPoint = Point3d.placeIn globalFrame (originPoint frame)
    , basis = Basis3d.placeIn globalFrame (basis frame)
    }

relativeTo ::
  Frame3d (global @ units) (Defines space) ->
  Frame3d (global @ units) (Defines local) ->
  Frame3d (space @ units) (Defines local)
relativeTo globalFrame frame =
  Frame3d
    { originPoint = Point3d.relativeTo globalFrame (originPoint frame)
    , basis = Basis3d.relativeTo globalFrame (basis frame)
    }

inverse :: Frame3d (global @ units) (Defines local) -> Frame3d (local @ units) (Defines global)
inverse frame = xyz |> relativeTo frame