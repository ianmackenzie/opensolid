module OpenSolid.Expression.Surface3d
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

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorSurface3d qualified as Expression.VectorSurface3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvPoint)
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Vector3d qualified as Vector3d

constant :: Point3d (space @ units) -> Expression UvPoint (Point3d (space @ units))
constant = Expression.constant

xyz ::
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Point3d (space @ units))
xyz = Expression.xyz

xCoordinate :: Expression UvPoint (Point3d (space @ units)) -> Expression UvPoint (Qty units)
xCoordinate = Expression.xCoordinate

yCoordinate :: Expression UvPoint (Point3d (space @ units)) -> Expression UvPoint (Qty units)
yCoordinate = Expression.yCoordinate

zCoordinate :: Expression UvPoint (Point3d (space @ units)) -> Expression UvPoint (Qty units)
zCoordinate = Expression.zCoordinate

interpolateFrom ::
  Expression UvPoint (Point3d (space @ units)) ->
  Expression UvPoint (Point3d (space @ units)) ->
  Expression UvPoint Float ->
  Expression UvPoint (Point3d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Expression UvPoint (Point3d (local @ units)) ->
  Expression UvPoint (Point3d (global @ units))
placeIn frame expression = do
  let i = Vector3d.unit (Frame3d.xDirection frame)
  let j = Vector3d.unit (Frame3d.yDirection frame)
  let k = Vector3d.unit (Frame3d.zDirection frame)
  constant (Frame3d.originPoint frame)
    + xCoordinate expression * Expression.VectorSurface3d.constant i
    + yCoordinate expression * Expression.VectorSurface3d.constant j
    + zCoordinate expression * Expression.VectorSurface3d.constant k

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Expression UvPoint (Point3d (global @ units)) ->
  Expression UvPoint (Point3d (local @ units))
relativeTo frame expression = do
  let i = Vector3d.unit (Frame3d.xDirection frame)
  let j = Vector3d.unit (Frame3d.yDirection frame)
  let k = Vector3d.unit (Frame3d.zDirection frame)
  let displacement = expression - constant (Frame3d.originPoint frame)
  xyz
    (displacement `dot` Expression.VectorSurface3d.constant i)
    (displacement `dot` Expression.VectorSurface3d.constant j)
    (displacement `dot` Expression.VectorSurface3d.constant k)

transformBy ::
  Transform3d tag (space @ units) ->
  Expression UvPoint (Point3d (space @ units)) ->
  Expression UvPoint (Point3d (space @ units))
transformBy (Transform3d p0 i j k) expression =
  constant p0
    + xCoordinate expression * Expression.VectorSurface3d.constant i
    + yCoordinate expression * Expression.VectorSurface3d.constant j
    + zCoordinate expression * Expression.VectorSurface3d.constant k
