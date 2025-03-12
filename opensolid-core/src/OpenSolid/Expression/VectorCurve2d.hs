module OpenSolid.Expression.VectorCurve2d
  ( constant
  , xy
  , xComponent
  , yComponent
  , squaredMagnitude
  , squaredMagnitude'
  , magnitude
  , interpolateFrom
  , placeIn
  , relativeTo
  , transformBy
  , placeOn
  )
where

import OpenSolid.Basis2d (Basis2d)
import OpenSolid.Basis2d qualified as Basis2d
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.PlanarBasis3d (PlanarBasis3d)
import OpenSolid.PlanarBasis3d qualified as PlanarBasis3d
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d

constant :: Vector2d (space @ units) -> Expression Float (Vector2d (space @ units))
constant = Expression.constant

xy ::
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Vector2d (space @ units))
xy = Expression.xy

xComponent :: Expression Float (Vector2d (space @ units)) -> Expression Float (Qty units)
xComponent = Expression.xComponent

yComponent :: Expression Float (Vector2d (space @ units)) -> Expression Float (Qty units)
yComponent = Expression.yComponent

squaredMagnitude' ::
  Expression Float (Vector2d (space @ units)) ->
  Expression Float (Qty (units :*: units))
squaredMagnitude' vector =
  Expression.squared' (xComponent vector) + Expression.squared' (yComponent vector)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression Float (Vector2d (space @ units1)) ->
  Expression Float (Qty units2)
squaredMagnitude = Units.specialize . squaredMagnitude'

magnitude :: Expression Float (Vector2d (space @ units)) -> Expression Float (Qty units)
magnitude = Expression.sqrt' . squaredMagnitude'

interpolateFrom ::
  Expression Float (Vector2d (space @ units)) ->
  Expression Float (Vector2d (space @ units)) ->
  Expression Float Float ->
  Expression Float (Vector2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Basis2d global (Defines local) ->
  Expression Float (Vector2d (local @ units)) ->
  Expression Float (Vector2d (global @ units))
placeIn basis vector = do
  let i = Vector2d.unit (Basis2d.xDirection basis)
  let j = Vector2d.unit (Basis2d.yDirection basis)
  xComponent vector * constant i + yComponent vector * constant j

relativeTo ::
  Basis2d global (Defines local) ->
  Expression Float (Vector2d (global @ units)) ->
  Expression Float (Vector2d (local @ units))
relativeTo basis vector = do
  let i = Vector2d.unit (Basis2d.xDirection basis)
  let j = Vector2d.unit (Basis2d.yDirection basis)
  xy (vector `dot` constant i) (vector `dot` constant j)

transformBy ::
  Transform2d a (space @ translationUnits) ->
  Expression Float (Vector2d (space @ units)) ->
  Expression Float (Vector2d (space @ units))
transformBy (Transform2d _ i j) vector =
  xComponent vector * constant i + yComponent vector * constant j

placeOn ::
  PlanarBasis3d space (Defines local) ->
  Expression Float (Vector2d (local @ units)) ->
  Expression Float (Vector3d (space @ units))
placeOn planarBasis vector = do
  let i = Vector3d.unit (PlanarBasis3d.xDirection planarBasis)
  let j = Vector3d.unit (PlanarBasis3d.yDirection planarBasis)
  xComponent vector * constant3d i + yComponent vector * constant3d j

constant3d :: Vector3d (space @ units) -> Expression Float (Vector3d (space @ units))
constant3d = Expression.constant
