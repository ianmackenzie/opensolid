module Expression.Surface2d
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

import Expression (Expression)
import Expression qualified
import Frame2d (Frame2d)
import Frame2d qualified
import OpenSolid
import Point2d (Point2d)
import Transform2d (Transform2d (Transform2d))
import Uv qualified


constant :: Point2d (space @ units) -> Expression Uv.Point (Point2d (space @ units))
constant = Expression.constant

xy ::
  Expression Uv.Point (Qty units) ->
  Expression Uv.Point (Qty units) ->
  Expression Uv.Point (Point2d (space @ units))
xy = Expression.xy

xCoordinate :: Expression Uv.Point (Point2d (space @ units)) -> Expression Uv.Point (Qty units)
xCoordinate = Expression.xCoordinate

yCoordinate :: Expression Uv.Point (Point2d (space @ units)) -> Expression Uv.Point (Qty units)
yCoordinate = Expression.yCoordinate

interpolateFrom ::
  Expression Uv.Point (Point2d (space @ units)) ->
  Expression Uv.Point (Point2d (space @ units)) ->
  Expression Uv.Point Float ->
  Expression Uv.Point (Point2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Expression Uv.Point (Point2d (local @ units)) ->
  Expression Uv.Point (Point2d (global @ units))
placeIn frame point =
  Frame2d.originPoint frame
    + xCoordinate point * Frame2d.xDirection frame
    + yCoordinate point * Frame2d.yDirection frame

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Expression Uv.Point (Point2d (global @ units)) ->
  Expression Uv.Point (Point2d (local @ units))
relativeTo frame point = do
  let displacement = point - Frame2d.originPoint frame
  xy (displacement <> Frame2d.xDirection frame) (displacement <> Frame2d.yDirection frame)

transformBy ::
  Transform2d tag (space @ units) ->
  Expression Uv.Point (Point2d (space @ units)) ->
  Expression Uv.Point (Point2d (space @ units))
transformBy (Transform2d p0 i j) point = p0 + xCoordinate point * i + yCoordinate point * j
