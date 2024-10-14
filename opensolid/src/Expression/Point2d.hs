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
import Expression.Expression1d qualified as Expression1d
import Frame2d (Frame2d)
import Frame2d qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Transform2d (Transform2d (Transform2d))
import Units qualified

constant :: Point2d (space @ units) -> Expression input (Point2d (space @ units))
constant (Point2d px py) =
  Expression.Point2d
    (Expression1d.constant (Units.coerce px))
    (Expression1d.constant (Units.coerce py))

xy ::
  Expression input (Qty units) ->
  Expression input (Qty units) ->
  Expression input (Point2d (space @ units))
xy (Expression.Qty x) (Expression.Qty y) = Expression.Point2d x y

xCoordinate :: Expression input (Point2d (space @ units)) -> Expression input (Qty units)
xCoordinate (Expression.Point2d px _) = Expression.Qty px

yCoordinate :: Expression input (Point2d (space @ units)) -> Expression input (Qty units)
yCoordinate (Expression.Point2d _ py) = Expression.Qty py

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
placeIn frame point =
  Frame2d.originPoint frame
    + xCoordinate point * Frame2d.xDirection frame
    + yCoordinate point * Frame2d.yDirection frame

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
transformBy (Transform2d p0 i j) point = p0 + xCoordinate point * i + yCoordinate point * j
