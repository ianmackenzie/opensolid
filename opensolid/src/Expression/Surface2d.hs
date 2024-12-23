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
import Expression.VectorSurface2d qualified
import Frame2d (Frame2d)
import Frame2d qualified
import OpenSolid
import Point2d (Point2d)
import SurfaceParameter (UvPoint)
import Transform2d (Transform2d (Transform2d))
import Vector2d qualified

constant :: Point2d (space @ units) -> Expression UvPoint (Point2d (space @ units))
constant = Expression.constant

xy ::
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Point2d (space @ units))
xy = Expression.xy

xCoordinate :: Expression UvPoint (Point2d (space @ units)) -> Expression UvPoint (Qty units)
xCoordinate = Expression.xCoordinate

yCoordinate :: Expression UvPoint (Point2d (space @ units)) -> Expression UvPoint (Qty units)
yCoordinate = Expression.yCoordinate

interpolateFrom ::
  Expression UvPoint (Point2d (space @ units)) ->
  Expression UvPoint (Point2d (space @ units)) ->
  Expression UvPoint Float ->
  Expression UvPoint (Point2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Expression UvPoint (Point2d (local @ units)) ->
  Expression UvPoint (Point2d (global @ units))
placeIn frame expression = do
  let i = Vector2d.unit (Frame2d.xDirection frame)
  let j = Vector2d.unit (Frame2d.yDirection frame)
  constant (Frame2d.originPoint frame)
    + xCoordinate expression * Expression.VectorSurface2d.constant i
    + yCoordinate expression * Expression.VectorSurface2d.constant j

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Expression UvPoint (Point2d (global @ units)) ->
  Expression UvPoint (Point2d (local @ units))
relativeTo frame expression = do
  let i = Vector2d.unit (Frame2d.xDirection frame)
  let j = Vector2d.unit (Frame2d.yDirection frame)
  let displacement = expression - constant (Frame2d.originPoint frame)
  xy
    (displacement <> Expression.VectorSurface2d.constant i)
    (displacement <> Expression.VectorSurface2d.constant j)

transformBy ::
  Transform2d tag (space @ units) ->
  Expression UvPoint (Point2d (space @ units)) ->
  Expression UvPoint (Point2d (space @ units))
transformBy (Transform2d p0 i j) expression =
  constant p0
    + xCoordinate expression * Expression.VectorSurface2d.constant i
    + yCoordinate expression * Expression.VectorSurface2d.constant j
