module Expression.Point2d
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
import Point2d (Point2d (Point2d))
import Transform2d (Transform2d (Transform2d))

constant :: Point2d (space @ units) -> Expression input (Point2d (space @ units))
constant (Point2d px py) = Expression.Point2d (Expression.constant px) (Expression.constant py)

xy ::
  Expression input (Qty units) ->
  Expression input (Qty units) ->
  Expression input (Point2d (space @ units))
xy = Expression.Point2d

xCoordinate :: Expression input (Point2d (space @ units)) -> Expression input (Qty units)
xCoordinate (Expression.Point2d px _) = px

yCoordinate :: Expression input (Point2d (space @ units)) -> Expression input (Qty units)
yCoordinate (Expression.Point2d _ py) = py

interpolateFrom ::
  Expression input (Point2d (space @ units)) ->
  Expression input (Point2d (space @ units)) ->
  Expression input Float ->
  Expression input (Point2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Expression input (Point2d (local @ units)) ->
  Expression input (Point2d (global @ units))
placeIn frame (Expression.Point2d x y) =
  Frame2d.originPoint frame + x * Frame2d.xDirection frame + y * Frame2d.yDirection frame

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Expression input (Point2d (global @ units)) ->
  Expression input (Point2d (local @ units))
relativeTo frame point = do
  let displacement = point - Frame2d.originPoint frame
  xy (displacement <> Frame2d.xDirection frame) (displacement <> Frame2d.yDirection frame)

transformBy ::
  Transform2d tag (space @ units) ->
  Expression input (Point2d (space @ units)) ->
  Expression input (Point2d (space @ units))
transformBy (Transform2d p0 i j) (Expression.Point2d x y) = p0 + x * i + y * j
