module OpenSolid.Expression.VectorCurve3d
  ( constant
  , xyz
  , xComponent
  , yComponent
  , zComponent
  , squaredMagnitude
  , squaredMagnitude'
  , magnitude
  , interpolateFrom
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , transformBy
  )
where

import Basis3d (Basis3d)
import Basis3d qualified
import Frame3d (Frame3d)
import Frame3d qualified
import OpenSolid.Prelude
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import Transform3d (Transform3d (Transform3d))
import Units qualified

constant :: Vector3d (space @ units) -> Expression Float (Vector3d (space @ units))
constant = Expression.constant

xyz ::
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Vector3d (space @ units))
xyz = Expression.xyz

xComponent :: Expression Float (Vector3d (space @ units)) -> Expression Float (Qty units)
xComponent = Expression.xComponent

yComponent :: Expression Float (Vector3d (space @ units)) -> Expression Float (Qty units)
yComponent = Expression.yComponent

zComponent :: Expression Float (Vector3d (space @ units)) -> Expression Float (Qty units)
zComponent = Expression.zComponent

squaredMagnitude' ::
  Expression Float (Vector3d (space @ units)) ->
  Expression Float (Qty (units :*: units))
squaredMagnitude' expression =
  Expression.squared' (xComponent expression)
    + Expression.squared' (yComponent expression)
    + Expression.squared' (zComponent expression)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression Float (Vector3d (space @ units1)) ->
  Expression Float (Qty units2)
squaredMagnitude = Units.specialize . squaredMagnitude'

magnitude :: Expression Float (Vector3d (space @ units)) -> Expression Float (Qty units)
magnitude = Expression.sqrt' . squaredMagnitude'

interpolateFrom ::
  Expression Float (Vector3d (space @ units)) ->
  Expression Float (Vector3d (space @ units)) ->
  Expression Float Float ->
  Expression Float (Vector3d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame3d (global @ originPointUnits) (Defines local) ->
  Expression Float (Vector3d (local @ units)) ->
  Expression Float (Vector3d (global @ units))
placeIn frame expression = placeInBasis (Frame3d.basis frame) expression

relativeTo ::
  Frame3d (global @ originPointUnits) (Defines local) ->
  Expression Float (Vector3d (global @ units)) ->
  Expression Float (Vector3d (local @ units))
relativeTo frame expression = relativeToBasis (Frame3d.basis frame) expression

placeInBasis ::
  Basis3d global (Defines local) ->
  Expression Float (Vector3d (local @ units)) ->
  Expression Float (Vector3d (global @ units))
placeInBasis basis expression = do
  let i = Vector3d.unit (Basis3d.xDirection basis)
  let j = Vector3d.unit (Basis3d.yDirection basis)
  let k = Vector3d.unit (Basis3d.zDirection basis)
  xComponent expression * constant i
    + yComponent expression * constant j
    + zComponent expression * constant k

relativeToBasis ::
  Basis3d global (Defines local) ->
  Expression Float (Vector3d (global @ units)) ->
  Expression Float (Vector3d (local @ units))
relativeToBasis basis expression = do
  let i = Vector3d.unit (Basis3d.xDirection basis)
  let j = Vector3d.unit (Basis3d.yDirection basis)
  let k = Vector3d.unit (Basis3d.zDirection basis)
  xyz
    (expression <> constant i)
    (expression <> constant j)
    (expression <> constant k)

transformBy ::
  Transform3d a (space @ translationUnits) ->
  Expression Float (Vector3d (space @ units)) ->
  Expression Float (Vector3d (space @ units))
transformBy (Transform3d _ i j k) expression =
  xComponent expression * constant i
    + yComponent expression * constant j
    + zComponent expression * constant k
