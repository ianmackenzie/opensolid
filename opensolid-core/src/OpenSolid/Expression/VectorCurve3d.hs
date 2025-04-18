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
  , transformBy
  )
where

import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Prelude
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d

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
  Basis3d global (Defines local) ->
  Expression Float (Vector3d (local @ units)) ->
  Expression Float (Vector3d (global @ units))
placeIn basis expression = do
  let i = Vector3d.unit (Basis3d.xDirection basis)
  let j = Vector3d.unit (Basis3d.yDirection basis)
  let k = Vector3d.unit (Basis3d.zDirection basis)
  xComponent expression * constant i
    + yComponent expression * constant j
    + zComponent expression * constant k

relativeTo ::
  Basis3d global (Defines local) ->
  Expression Float (Vector3d (global @ units)) ->
  Expression Float (Vector3d (local @ units))
relativeTo basis expression = do
  let i = Vector3d.unit (Basis3d.xDirection basis)
  let j = Vector3d.unit (Basis3d.yDirection basis)
  let k = Vector3d.unit (Basis3d.zDirection basis)
  xyz
    (expression `dot` constant i)
    (expression `dot` constant j)
    (expression `dot` constant k)

transformBy ::
  Transform3d a (space @ translationUnits) ->
  Expression Float (Vector3d (space @ units)) ->
  Expression Float (Vector3d (space @ units))
transformBy (Transform3d _ i j k) expression =
  xComponent expression * constant i
    + yComponent expression * constant j
    + zComponent expression * constant k
