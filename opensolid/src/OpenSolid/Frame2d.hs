module OpenSolid.Frame2d
  ( Frame2d
  , coerce
  , xy
  , at
  , originPoint
  , basis
  , xDirection
  , yDirection
  , xAxis
  , yAxis
  , fromXAxis
  , fromYAxis
  , placeIn
  , relativeTo
  , inverse
  )
where

import OpenSolid.Basis2d (Basis2d)
import OpenSolid.Basis2d qualified as Basis2d
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Prelude
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d

type role Frame2d nominal nominal

type Frame2d :: CoordinateSystem -> LocalSpace -> Type
data Frame2d coordinateSystem defines where
  Frame2d ::
    { originPoint :: Point2d (space @ units)
    , basis :: Basis2d space defines
    } ->
    Frame2d (space @ units) defines

deriving instance Eq (Frame2d (space @ units) defines)

deriving instance Show (Frame2d (space @ units) defines)

coerce :: Frame2d (space @ units) defines1 -> Frame2d (space @ units) defines2
coerce Frame2d{originPoint, basis} = Frame2d{originPoint, basis = Basis2d.coerce basis}

xDirection :: Frame2d (space @ units) defines -> Direction2d space
xDirection frame = Basis2d.xDirection (basis frame)

yDirection :: Frame2d (space @ units) defines -> Direction2d space
yDirection frame = Basis2d.yDirection (basis frame)

xy :: Frame2d (space @ units) defines
xy = Frame2d Point2d.origin Basis2d.xy

at :: Point2d (space @ units) -> Basis2d space defines -> Frame2d (space @ units) defines
at point basis = Frame2d point basis

fromXAxis :: Axis2d (space @ units) -> Frame2d (space @ units) defines
fromXAxis axis = Frame2d (Axis2d.originPoint axis) (Basis2d.fromXDirection (Axis2d.direction axis))

fromYAxis :: Axis2d (space @ units) -> Frame2d (space @ units) defines
fromYAxis axis = Frame2d (Axis2d.originPoint axis) (Basis2d.fromYDirection (Axis2d.direction axis))

xAxis :: Frame2d (space @ units) defines -> Axis2d (space @ units)
xAxis frame = Axis2d.through (originPoint frame) (xDirection frame)

yAxis :: Frame2d (space @ units) defines -> Axis2d (space @ units)
yAxis frame = Axis2d.through (originPoint frame) (yDirection frame)

placeIn ::
  Frame2d (global @ units) (Defines space) ->
  Frame2d (space @ units) (Defines local) ->
  Frame2d (global @ units) (Defines local)
placeIn globalFrame frame =
  Frame2d
    { originPoint = Point2d.placeIn globalFrame (originPoint frame)
    , basis = Basis2d.placeIn globalFrame (basis frame)
    }

relativeTo ::
  Frame2d (global @ units) (Defines space) ->
  Frame2d (global @ units) (Defines local) ->
  Frame2d (space @ units) (Defines local)
relativeTo globalFrame frame =
  Frame2d
    { originPoint = Point2d.relativeTo globalFrame (originPoint frame)
    , basis = Basis2d.relativeTo globalFrame (basis frame)
    }

inverse :: Frame2d (global @ units) (Defines local) -> Frame2d (local @ units) (Defines global)
inverse frame = xy |> relativeTo frame
