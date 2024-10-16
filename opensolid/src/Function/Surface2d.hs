module Function.Surface2d
  ( constant
  , xy
  , xCoordinate
  , yCoordinate
  , interpolateFrom
  , placeIn
  , relativeTo
  , transformBy
  )
where

import Function (Function)
import Function qualified
import Frame2d (Frame2d)
import Frame2d qualified
import OpenSolid
import Point2d (Point2d)
import Transform2d (Transform2d (Transform2d))
import Uv qualified


constant :: Point2d (space @ units) -> Function Uv.Point (Point2d (space @ units))
constant = Function.constant

xy ::
  Function Uv.Point (Qty units) ->
  Function Uv.Point (Qty units) ->
  Function Uv.Point (Point2d (space @ units))
xy = Function.xy

xCoordinate :: Function Uv.Point (Point2d (space @ units)) -> Function Uv.Point (Qty units)
xCoordinate = Function.xCoordinate

yCoordinate :: Function Uv.Point (Point2d (space @ units)) -> Function Uv.Point (Qty units)
yCoordinate = Function.yCoordinate

interpolateFrom ::
  Function Uv.Point (Point2d (space @ units)) ->
  Function Uv.Point (Point2d (space @ units)) ->
  Function Uv.Point Float ->
  Function Uv.Point (Point2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Function Uv.Point (Point2d (local @ units)) ->
  Function Uv.Point (Point2d (global @ units))
placeIn frame point =
  Frame2d.originPoint frame
    + xCoordinate point * Frame2d.xDirection frame
    + yCoordinate point * Frame2d.yDirection frame

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Function Uv.Point (Point2d (global @ units)) ->
  Function Uv.Point (Point2d (local @ units))
relativeTo frame point = do
  let displacement = point - Frame2d.originPoint frame
  xy (displacement <> Frame2d.xDirection frame) (displacement <> Frame2d.yDirection frame)

transformBy ::
  Transform2d tag (space @ units) ->
  Function Uv.Point (Point2d (space @ units)) ->
  Function Uv.Point (Point2d (space @ units))
transformBy (Transform2d p0 i j) point = p0 + xCoordinate point * i + yCoordinate point * j
