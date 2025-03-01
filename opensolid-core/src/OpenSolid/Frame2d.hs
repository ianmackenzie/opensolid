module OpenSolid.Frame2d
  ( Frame2d (Frame2d)
  , coerce
  , xy
  , atPoint
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
  , placeOn
  , inverse
  )
where

import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Basis2d (Basis2d)
import OpenSolid.Basis2d qualified as Basis2d
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Basis2d (Basis2d)
  , Frame2d (Frame2d)
  , PlanarBasis3d (PlanarBasis3d)
  , Plane3d (Plane3d)
  )

originPoint :: Frame2d (space @ units) defines -> Point2d (space @ units)
originPoint (Frame2d p0 _) = p0

basis :: Frame2d (space @ units) defines -> Basis2d space defines
basis (Frame2d _ b) = b

coerce :: Frame2d (space @ units) defines1 -> Frame2d (space @ units) defines2
coerce (Frame2d p0 b) = Frame2d p0 (Basis2d.coerce b)

xDirection :: Frame2d (space @ units) defines -> Direction2d space
xDirection frame = Basis2d.xDirection (basis frame)

yDirection :: Frame2d (space @ units) defines -> Direction2d space
yDirection frame = Basis2d.yDirection (basis frame)

xy :: Frame2d (space @ units) defines
xy = atPoint Point2d.origin

atPoint :: Point2d (space @ units) -> Frame2d (space @ units) defines
atPoint p0 = Frame2d p0 Basis2d.xy

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
    (Point2d.placeIn globalFrame (originPoint frame))
    (Basis2d.placeIn globalFrame (basis frame))

relativeTo ::
  Frame2d (global @ units) (Defines space) ->
  Frame2d (global @ units) (Defines local) ->
  Frame2d (space @ units) (Defines local)
relativeTo globalFrame frame =
  Frame2d
    (Point2d.relativeTo globalFrame (originPoint frame))
    (Basis2d.relativeTo globalFrame (basis frame))

placeOn ::
  Plane3d (space @ units) (Defines local) ->
  Frame2d (local @ units) defines ->
  Plane3d (space @ units) defines
placeOn plane (Frame2d p0 (Basis2d i j)) = do
  Plane3d (Point2d.placeOn plane p0) $
    PlanarBasis3d (Direction2d.placeOn plane i) (Direction2d.placeOn plane j)

inverse :: Frame2d (global @ units) (Defines local) -> Frame2d (local @ units) (Defines global)
inverse frame = xy |> relativeTo frame
