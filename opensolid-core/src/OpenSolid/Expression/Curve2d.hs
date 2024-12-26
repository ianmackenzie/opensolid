module OpenSolid.Expression.Curve2d
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

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Vector2d qualified as Vector2d

constant :: Point2d (space @ units) -> Expression Float (Point2d (space @ units))
constant = Expression.constant

xy ::
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Point2d (space @ units))
xy = Expression.xy

xCoordinate :: Expression Float (Point2d (space @ units)) -> Expression Float (Qty units)
xCoordinate = Expression.xCoordinate

yCoordinate :: Expression Float (Point2d (space @ units)) -> Expression Float (Qty units)
yCoordinate = Expression.yCoordinate

interpolateFrom ::
  Expression Float (Point2d (space @ units)) ->
  Expression Float (Point2d (space @ units)) ->
  Expression Float Float ->
  Expression Float (Point2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Expression Float (Point2d (local @ units)) ->
  Expression Float (Point2d (global @ units))
placeIn frame expression = do
  let i = Vector2d.unit (Frame2d.xDirection frame)
  let j = Vector2d.unit (Frame2d.yDirection frame)
  constant (Frame2d.originPoint frame)
    + xCoordinate expression * Expression.VectorCurve2d.constant i
    + yCoordinate expression * Expression.VectorCurve2d.constant j

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Expression Float (Point2d (global @ units)) ->
  Expression Float (Point2d (local @ units))
relativeTo frame expression = do
  let i = Vector2d.unit (Frame2d.xDirection frame)
  let j = Vector2d.unit (Frame2d.yDirection frame)
  let displacement = expression - constant (Frame2d.originPoint frame)
  xy
    (displacement <> Expression.VectorCurve2d.constant i)
    (displacement <> Expression.VectorCurve2d.constant j)

transformBy ::
  Transform2d tag (space @ units) ->
  Expression Float (Point2d (space @ units)) ->
  Expression Float (Point2d (space @ units))
transformBy (Transform2d p0 i j) expression =
  constant p0
    + xCoordinate expression * Expression.VectorCurve2d.constant i
    + yCoordinate expression * Expression.VectorCurve2d.constant j
