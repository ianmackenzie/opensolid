module OpenSolid.Frame2d
  ( Frame2d (Frame2d)
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
  , on
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
  ( Frame2d (Frame2d)
  , Orientation2d (Orientation2d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  )

originPoint :: Frame2d (space @ units) defines -> Point2d (space @ units)
originPoint (Frame2d p0 _) = p0

orientation :: Frame2d (space @ units) defines -> Orientation2d space defines
orientation (Frame2d _ o) = o

coerce :: Frame2d (space1 @ units1) defines1 -> Frame2d (space2 @ units2) defines2
coerce (Frame2d p o) = Frame2d (Point2d.coerce p) (Orientation2d.coerce o)

erase :: Frame2d (space @ units) defines -> Frame2d (space @ Unitless) defines
erase = coerce

xDirection :: Frame2d (space @ units) defines -> Direction2d space
xDirection frame = Orientation2d.xDirection (orientation frame)

yDirection :: Frame2d (space @ units) defines -> Direction2d space
yDirection frame = Orientation2d.yDirection (orientation frame)

xy :: Frame2d (space @ units) defines
xy = atPoint Point2d.origin

atPoint :: Point2d (space @ units) -> Frame2d (space @ units) defines
atPoint p0 = Frame2d p0 Orientation2d.xy

fromXAxis :: Axis2d (space @ units) -> Frame2d (space @ units) defines
fromXAxis axis = Frame2d (Axis2d.originPoint axis) (Orientation2d.fromXDirection (Axis2d.direction axis))

fromYAxis :: Axis2d (space @ units) -> Frame2d (space @ units) defines
fromYAxis axis = Frame2d (Axis2d.originPoint axis) (Orientation2d.fromYDirection (Axis2d.direction axis))

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
    (Point2d.placeIn globalFrame (originPoint frame))
    (Orientation2d.placeIn (orientation globalFrame) (orientation frame))

relativeTo ::
  Frame2d (global @ units) (Defines space) ->
  Frame2d (global @ units) (Defines local) ->
  Frame2d (space @ units) (Defines local)
relativeTo globalFrame frame =
  Frame2d
    (Point2d.relativeTo globalFrame (originPoint frame))
    (Orientation2d.relativeTo (orientation globalFrame) (orientation frame))

on ::
  Plane3d (space @ units) (Defines local) ->
  Frame2d (local @ units) defines ->
  Plane3d (space @ units) defines
on plane (Frame2d p0 (Orientation2d i j)) = do
  let Plane3d _ planeOrientation = plane
  Plane3d (Point2d.on plane p0) $
    PlaneOrientation3d (Direction2d.on planeOrientation i) (Direction2d.on planeOrientation j)

inverse :: Frame2d (global @ units) (Defines local) -> Frame2d (local @ units) (Defines global)
inverse frame = xy |> relativeTo frame
