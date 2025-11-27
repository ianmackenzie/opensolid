module OpenSolid.Frame2d
  ( Frame2d (Frame2d, originPoint, orientation)
  , coerce
  , erase
  , xy
  , atPoint
  , originPoint
  , orientation
  , xDirection
  , yDirection
  , xAxis
  , yAxis
  , fromXAxis
  , fromYAxis
  , placeIn
  , relativeTo
  , placeOn
  , inverse
  )
where

import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Orientation2d (Orientation2d)
import OpenSolid.Orientation2d qualified as Orientation2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Frame2d (Frame2d, orientation, originPoint)
  , Orientation2d (Orientation2d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  )

originPoint :: Frame2d units global local -> Point2d units global
originPoint (Frame2d p0 _) = p0

orientation :: Frame2d units global local -> Orientation2d global
orientation (Frame2d _ o) = o

coerce :: Frame2d units1 global1 local1 -> Frame2d units2 global2 local2
coerce (Frame2d p o) = Frame2d (Point2d.coerce p) (Orientation2d.coerce o)

erase :: Frame2d units global local -> Frame2d Unitless global local
erase = coerce

xDirection :: Frame2d units global local -> Direction2d global
xDirection frame = Orientation2d.xDirection (orientation frame)

yDirection :: Frame2d units global local -> Direction2d global
yDirection frame = Orientation2d.yDirection (orientation frame)

xy :: Frame2d units global local
xy = atPoint Point2d.origin

atPoint :: Point2d units global -> Frame2d units global local
atPoint p0 = Frame2d p0 Orientation2d.horizontal

fromXAxis :: Axis2d units global -> Frame2d units global local
fromXAxis axis =
  Frame2d (Axis2d.originPoint axis) (Orientation2d.fromXDirection (Axis2d.direction axis))

fromYAxis :: Axis2d units global -> Frame2d units global local
fromYAxis axis =
  Frame2d (Axis2d.originPoint axis) (Orientation2d.fromYDirection (Axis2d.direction axis))

xAxis :: Frame2d units global local -> Axis2d units global
xAxis frame = Axis2d.through (originPoint frame) (xDirection frame)

yAxis :: Frame2d units global local -> Axis2d units global
yAxis frame = Axis2d.through (originPoint frame) (yDirection frame)

placeIn ::
  Frame2d units space1 space2 ->
  Frame2d units space2 space3 ->
  Frame2d units space1 space3
placeIn globalFrame frame =
  Frame2d
    (Point2d.placeIn globalFrame (originPoint frame))
    (Orientation2d.placeIn globalFrame (orientation frame))

relativeTo ::
  Frame2d units space1 space2 ->
  Frame2d units space1 space3 ->
  Frame2d units space2 space3
relativeTo globalFrame frame =
  Frame2d
    (Point2d.relativeTo globalFrame (originPoint frame))
    (Orientation2d.relativeTo globalFrame (orientation frame))

placeOn :: Plane3d space1 space2 -> Frame2d Meters space2 space3 -> Plane3d space1 space3
placeOn plane (Frame2d p0 (Orientation2d i j)) =
  Plane3d (Point2d.placeOn plane p0) $
    PlaneOrientation3d (Direction2d.placeOn plane i) (Direction2d.placeOn plane j)

inverse :: Frame2d units global local -> Frame2d units local global
inverse frame = relativeTo frame xy
