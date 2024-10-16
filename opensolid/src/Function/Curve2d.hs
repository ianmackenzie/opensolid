module Function.Curve2d
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

constant :: Point2d (space @ units) -> Function Float (Point2d (space @ units))
constant = Function.constant

xy ::
  Function Float (Qty units) ->
  Function Float (Qty units) ->
  Function Float (Point2d (space @ units))
xy = Function.xy

xCoordinate :: Function Float (Point2d (space @ units)) -> Function Float (Qty units)
xCoordinate = Function.xCoordinate

yCoordinate :: Function Float (Point2d (space @ units)) -> Function Float (Qty units)
yCoordinate = Function.yCoordinate

interpolateFrom ::
  Function Float (Point2d (space @ units)) ->
  Function Float (Point2d (space @ units)) ->
  Function Float Float ->
  Function Float (Point2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Function Float (Point2d (local @ units)) ->
  Function Float (Point2d (global @ units))
placeIn frame point =
  Frame2d.originPoint frame
    + xCoordinate point * Frame2d.xDirection frame
    + yCoordinate point * Frame2d.yDirection frame

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Function Float (Point2d (global @ units)) ->
  Function Float (Point2d (local @ units))
relativeTo frame point = do
  let displacement = point - Frame2d.originPoint frame
  xy (displacement <> Frame2d.xDirection frame) (displacement <> Frame2d.yDirection frame)

transformBy ::
  Transform2d tag (space @ units) ->
  Function Float (Point2d (space @ units)) ->
  Function Float (Point2d (space @ units))
transformBy (Transform2d p0 i j) point = p0 + xCoordinate point * i + yCoordinate point * j
