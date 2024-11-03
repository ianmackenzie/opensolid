module Expression.VectorCurve2d
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

import Basis2d (Basis2d)
import Basis2d qualified
import Expression (Expression)
import Expression qualified
import Frame2d (Frame2d)
import Frame2d qualified
import OpenSolid
import Transform2d (Transform2d (Transform2d))
import Units qualified
import Vector2d (Vector2d)
import Vector2d qualified

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
