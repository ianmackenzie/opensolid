module Frame2d
  ( Frame2d
  , xy
  , withOriginPoint
  , originPoint
  , basis
  , xDirection
  , yDirection
  , xAxis
  , yAxis
  , withXDirection
  , withYDirection
  , fromXAxis
  , fromYAxis
  , placeIn
  , relativeTo
  )
where

import Axis2d (Axis2d)
import Axis2d qualified
import Basis2d (Basis2d)
import Basis2d qualified
import Direction2d (Direction2d)
import OpenSolid
import Point2d (Point2d)
import Point2d qualified

type role Frame2d nominal nominal

type Frame2d :: CoordinateSystem -> LocalSpace -> Type
data Frame2d coordinateSystem defines where
  Frame2d ::
    { originPoint :: Point2d (space @ units)
    , basis :: Basis2d space defines
    } ->
    Frame2d (space @ units) defines

deriving instance Show (Frame2d coordinateSystem defines)

xDirection :: Frame2d (space @ units) defines -> Direction2d space
xDirection frame = Basis2d.xDirection (basis frame)

yDirection :: Frame2d (space @ units) defines -> Direction2d space
yDirection frame = Basis2d.yDirection (basis frame)

xy :: Frame2d (space @ units) defines
xy = Frame2d Point2d.origin Basis2d.xy

withOriginPoint :: Point2d (space @ units) -> Frame2d (space @ units) defines
withOriginPoint point = Frame2d point Basis2d.xy

withXDirection :: Direction2d space -> Point2d (space @ units) -> Frame2d (space @ units) defines
withXDirection dx p0 = Frame2d{originPoint = p0, basis = Basis2d.fromXDirection dx}

withYDirection :: Direction2d space -> Point2d (space @ units) -> Frame2d (space @ units) defines
withYDirection dy p0 = Frame2d{originPoint = p0, basis = Basis2d.fromYDirection dy}

fromXAxis :: Axis2d (space @ units) -> Frame2d (space @ units) defines
fromXAxis axis = withXDirection (Axis2d.direction axis) (Axis2d.originPoint axis)

fromYAxis :: Axis2d (space @ units) -> Frame2d (space @ units) defines
fromYAxis axis = withYDirection (Axis2d.direction axis) (Axis2d.originPoint axis)

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
