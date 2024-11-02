module Expression.VectorCurve3d
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
import Expression (Expression)
import Expression qualified
import Frame3d (Frame3d)
import Frame3d qualified
import OpenSolid
import Transform3d (Transform3d (Transform3d))
import Units qualified
import Vector3d (Vector3d)

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
placeInBasis basis expression =
  xComponent expression * Basis3d.xDirection basis
    + yComponent expression * Basis3d.yDirection basis
    + zComponent expression * Basis3d.zDirection basis

relativeToBasis ::
  Basis3d global (Defines local) ->
  Expression Float (Vector3d (global @ units)) ->
  Expression Float (Vector3d (local @ units))
relativeToBasis basis expression =
  xyz
    (expression <> Basis3d.xDirection basis)
    (expression <> Basis3d.yDirection basis)
    (expression <> Basis3d.zDirection basis)

transformBy ::
  Transform3d a (space @ translationUnits) ->
  Expression Float (Vector3d (space @ units)) ->
  Expression Float (Vector3d (space @ units))
transformBy (Transform3d _ i j k) expression =
  xComponent expression * i + yComponent expression * j + zComponent expression * k
