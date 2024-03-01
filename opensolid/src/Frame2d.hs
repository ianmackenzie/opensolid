module Frame2d
  ( Frame2d
  , atOrigin
  , atPoint
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

atOrigin :: Frame2d (space @ units) defines
atOrigin = atPoint Point2d.origin

atPoint :: Point2d (space @ units) -> Frame2d (space @ units) defines
atPoint point = Frame2d{originPoint = point, basis = Basis2d.xy}

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
