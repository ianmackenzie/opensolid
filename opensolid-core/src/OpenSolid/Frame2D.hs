module OpenSolid.Frame2D
  ( Frame2D (Frame2D, originPoint, orientation)
  , coerce
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

import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Orientation2D (Orientation2D)
import OpenSolid.Orientation2D qualified as Orientation2D
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Frame2D (Frame2D, orientation, originPoint)
  , Orientation2D (Orientation2D)
  , Plane3D (Plane3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  )

originPoint :: Frame2D units global local -> Point2D units global
originPoint (Frame2D p0 _) = p0

orientation :: Frame2D units global local -> Orientation2D global
orientation (Frame2D _ o) = o

coerce :: Frame2D units1 global1 local1 -> Frame2D units2 global2 local2
coerce (Frame2D p o) = Frame2D (Point2D.coerce p) (Orientation2D.coerce o)

xDirection :: Frame2D units global local -> Direction2D global
xDirection frame = Orientation2D.xDirection (orientation frame)

yDirection :: Frame2D units global local -> Direction2D global
yDirection frame = Orientation2D.yDirection (orientation frame)

xy :: Frame2D units global local
xy = atPoint Point2D.origin

atPoint :: Point2D units global -> Frame2D units global local
atPoint p0 = Frame2D p0 Orientation2D.horizontal

fromXAxis :: Axis2D units global -> Frame2D units global local
fromXAxis axis =
  Frame2D (Axis2D.originPoint axis) (Orientation2D.fromXDirection (Axis2D.direction axis))

fromYAxis :: Axis2D units global -> Frame2D units global local
fromYAxis axis =
  Frame2D (Axis2D.originPoint axis) (Orientation2D.fromYDirection (Axis2D.direction axis))

xAxis :: Frame2D units global local -> Axis2D units global
xAxis frame = Axis2D.through (originPoint frame) (xDirection frame)

yAxis :: Frame2D units global local -> Axis2D units global
yAxis frame = Axis2D.through (originPoint frame) (yDirection frame)

placeIn ::
  Frame2D units space1 space2 ->
  Frame2D units space2 space3 ->
  Frame2D units space1 space3
placeIn globalFrame frame =
  Frame2D
    (Point2D.placeIn globalFrame (originPoint frame))
    (Orientation2D.placeIn globalFrame (orientation frame))

relativeTo ::
  Frame2D units space1 space2 ->
  Frame2D units space1 space3 ->
  Frame2D units space2 space3
relativeTo globalFrame frame =
  Frame2D
    (Point2D.relativeTo globalFrame (originPoint frame))
    (Orientation2D.relativeTo globalFrame (orientation frame))

placeOn :: Plane3D space1 space2 -> Frame2D Meters space2 space3 -> Plane3D space1 space3
placeOn plane (Frame2D p0 (Orientation2D i j)) =
  Plane3D (Point2D.placeOn plane p0) $
    PlaneOrientation3D (Direction2D.placeOn plane i) (Direction2D.placeOn plane j)

inverse :: Frame2D units global local -> Frame2D units local global
inverse frame = relativeTo frame xy
