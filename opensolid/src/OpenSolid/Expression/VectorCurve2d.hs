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
  , placeInBasis
  , relativeToBasis
  , transformBy
  )
where

import OpenSolid.Basis2d (Basis2d)
import OpenSolid.Basis2d qualified as Basis2d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Prelude
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d qualified as Vector2d

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
  Frame2d (global @ originPointUnits) (Defines local) ->
  Expression Float (Vector2d (local @ units)) ->
  Expression Float (Vector2d (global @ units))
placeIn frame expression = placeInBasis (Frame2d.basis frame) expression

relativeTo ::
  Frame2d (global @ originPointUnits) (Defines local) ->
  Expression Float (Vector2d (global @ units)) ->
  Expression Float (Vector2d (local @ units))
relativeTo frame expression = relativeToBasis (Frame2d.basis frame) expression

placeInBasis ::
  Basis2d global (Defines local) ->
  Expression Float (Vector2d (local @ units)) ->
  Expression Float (Vector2d (global @ units))
placeInBasis basis vector = do
  let i = Vector2d.unit (Basis2d.xDirection basis)
  let j = Vector2d.unit (Basis2d.yDirection basis)
  xComponent vector * constant i + yComponent vector * constant j

relativeToBasis ::
  Basis2d global (Defines local) ->
  Expression Float (Vector2d (global @ units)) ->
  Expression Float (Vector2d (local @ units))
relativeToBasis basis vector = do
  let i = Vector2d.unit (Basis2d.xDirection basis)
  let j = Vector2d.unit (Basis2d.yDirection basis)
  xy (vector <> constant i) (vector <> constant j)

transformBy ::
  Transform2d a (space @ translationUnits) ->
  Expression Float (Vector2d (space @ units)) ->
  Expression Float (Vector2d (space @ units))
transformBy (Transform2d _ i j) vector =
  xComponent vector * constant i + yComponent vector * constant j
