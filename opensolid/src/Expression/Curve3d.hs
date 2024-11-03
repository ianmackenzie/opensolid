module Expression.Curve3d
  ( constant
  , xyz
  , xCoordinate
  , yCoordinate
  , zCoordinate
  , interpolateFrom
  , placeIn
  , relativeTo
  , transformBy
  )
where

import Expression (Expression)
import Expression qualified
import Expression.VectorCurve3d qualified
import Frame3d (Frame3d)
import Frame3d qualified
import OpenSolid
import Point3d (Point3d)
import Transform3d (Transform3d (Transform3d))
import Vector3d qualified

constant :: Point3d (space @ units) -> Expression Float (Point3d (space @ units))
constant = Expression.constant

xyz ::
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Point3d (space @ units))
xyz = Expression.xyz

xCoordinate :: Expression Float (Point3d (space @ units)) -> Expression Float (Qty units)
xCoordinate = Expression.xCoordinate

yCoordinate :: Expression Float (Point3d (space @ units)) -> Expression Float (Qty units)
yCoordinate = Expression.yCoordinate

zCoordinate :: Expression Float (Point3d (space @ units)) -> Expression Float (Qty units)
zCoordinate = Expression.zCoordinate

interpolateFrom ::
  Expression Float (Point3d (space @ units)) ->
  Expression Float (Point3d (space @ units)) ->
  Expression Float Float ->
  Expression Float (Point3d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Expression Float (Point3d (local @ units)) ->
  Expression Float (Point3d (global @ units))
placeIn frame expression = do
  let i = Vector3d.unit (Frame3d.xDirection frame)
  let j = Vector3d.unit (Frame3d.yDirection frame)
  let k = Vector3d.unit (Frame3d.zDirection frame)
  constant (Frame3d.originPoint frame)
    + xCoordinate expression * Expression.VectorCurve3d.constant i
    + yCoordinate expression * Expression.VectorCurve3d.constant j
    + zCoordinate expression * Expression.VectorCurve3d.constant k

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Expression Float (Point3d (global @ units)) ->
  Expression Float (Point3d (local @ units))
relativeTo frame expression = do
  let i = Vector3d.unit (Frame3d.xDirection frame)
  let j = Vector3d.unit (Frame3d.yDirection frame)
  let k = Vector3d.unit (Frame3d.zDirection frame)
  let displacement = expression - constant (Frame3d.originPoint frame)
  xyz
    (displacement <> Expression.VectorCurve3d.constant i)
    (displacement <> Expression.VectorCurve3d.constant j)
    (displacement <> Expression.VectorCurve3d.constant k)

transformBy ::
  Transform3d tag (space @ units) ->
  Expression Float (Point3d (space @ units)) ->
  Expression Float (Point3d (space @ units))
transformBy (Transform3d p0 i j k) expression =
  constant p0
    + xCoordinate expression * Expression.VectorCurve3d.constant i
    + yCoordinate expression * Expression.VectorCurve3d.constant j
    + zCoordinate expression * Expression.VectorCurve3d.constant k
