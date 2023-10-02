module Frame2d
  ( Frame2d
  , atOrigin
  , atPoint
  , originPoint
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
import Direction2d (Direction2d)
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified

type role Frame2d nominal nominal

type Frame2d :: CoordinateSystem -> LocalSpace -> Type
data Frame2d coordinateSystem defines where
  Frame2d ::
    Point2d (space @ units) ->
    Direction2d space ->
    Direction2d space ->
    Frame2d (space @ units) defines

atOrigin :: Frame2d (space @ units) defines
atOrigin = atPoint Point2d.origin

atPoint :: Point2d (space @ units) -> Frame2d (space @ units) defines
atPoint point = Frame2d point Direction2d.x Direction2d.y

withXDirection :: Direction2d space -> Point2d (space @ units) -> Frame2d (space @ units) defines
withXDirection dx p0 = Frame2d p0 dx (Direction2d.rotateLeft dx)

withYDirection :: Direction2d space -> Point2d (space @ units) -> Frame2d (space @ units) defines
withYDirection dy p0 = Frame2d p0 (Direction2d.rotateRight dy) dy

fromXAxis :: Axis2d (space @ units) -> Frame2d (space @ units) defines
fromXAxis axis = withXDirection (Axis2d.direction axis) (Axis2d.originPoint axis)

fromYAxis :: Axis2d (space @ units) -> Frame2d (space @ units) defines
fromYAxis axis = withYDirection (Axis2d.direction axis) (Axis2d.originPoint axis)

originPoint :: Frame2d (space @ units) defines -> Point2d (space @ units)
originPoint (Frame2d p0 _ _) = p0

xDirection :: Frame2d (space @ units) defines -> Direction2d space
xDirection (Frame2d _ dx _) = dx

yDirection :: Frame2d (space @ units) defines -> Direction2d space
yDirection (Frame2d _ _ dy) = dy

xAxis :: Frame2d (space @ units) defines -> Axis2d (space @ units)
xAxis frame = Axis2d.through (originPoint frame) (xDirection frame)

yAxis :: Frame2d (space @ units) defines -> Axis2d (space @ units)
yAxis frame = Axis2d.through (originPoint frame) (yDirection frame)
