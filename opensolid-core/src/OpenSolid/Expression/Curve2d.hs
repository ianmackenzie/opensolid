module OpenSolid.Expression.Curve2d
  ( constant
  , xy
  , xCoordinate
  , yCoordinate
  , interpolateFrom
  , placeIn
  , relativeTo
  , transformBy
  , placeOn
  )
where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve3d qualified as Expression.Curve3d
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.Expression.VectorCurve3d qualified as Expression.VectorCurve3d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d qualified as Vector3d

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

placeOn ::
  Plane3d (global @ units) (Defines local) ->
  Expression Float (Point2d (local @ units)) ->
  Expression Float (Point3d (global @ units))
placeOn plane expression = do
  let i = Vector3d.unit (Plane3d.xDirection plane)
  let j = Vector3d.unit (Plane3d.yDirection plane)
  Expression.Curve3d.constant (Plane3d.originPoint plane)
    + xCoordinate expression * Expression.VectorCurve3d.constant i
    + yCoordinate expression * Expression.VectorCurve3d.constant j

transformBy ::
  Transform2d tag (space @ units) ->
  Expression Float (Point2d (space @ units)) ->
  Expression Float (Point2d (space @ units))
transformBy (Transform2d p0 i j) expression =
  constant p0
    + xCoordinate expression * Expression.VectorCurve2d.constant i
    + yCoordinate expression * Expression.VectorCurve2d.constant j
