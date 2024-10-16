module Expression.VectorSurface2d
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
import Uv qualified
import Vector2d (Vector2d)

constant :: Vector2d (space @ units) -> Expression Uv.Point (Vector2d (space @ units))
constant = Expression.constant

xy ::
  Expression Uv.Point (Qty units) ->
  Expression Uv.Point (Qty units) ->
  Expression Uv.Point (Vector2d (space @ units))
xy = Expression.xy

xComponent :: Expression Uv.Point (Vector2d (space @ units)) -> Expression Uv.Point (Qty units)
xComponent = Expression.xComponent

yComponent :: Expression Uv.Point (Vector2d (space @ units)) -> Expression Uv.Point (Qty units)
yComponent = Expression.yComponent

squaredMagnitude' ::
  Expression Uv.Point (Vector2d (space @ units)) ->
  Expression Uv.Point (Qty (units :*: units))
squaredMagnitude' vector =
  Expression.squared' (xComponent vector) + Expression.squared' (yComponent vector)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression Uv.Point (Vector2d (space @ units1)) ->
  Expression Uv.Point (Qty units2)
squaredMagnitude = Units.specialize . squaredMagnitude'

magnitude :: Expression Uv.Point (Vector2d (space @ units)) -> Expression Uv.Point (Qty units)
magnitude = Expression.sqrt' . squaredMagnitude'

interpolateFrom ::
  Expression Uv.Point (Vector2d (space @ units)) ->
  Expression Uv.Point (Vector2d (space @ units)) ->
  Expression Uv.Point Float ->
  Expression Uv.Point (Vector2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (global @ originPointUnits) (Defines local) ->
  Expression Uv.Point (Vector2d (local @ units)) ->
  Expression Uv.Point (Vector2d (global @ units))
placeIn frame expression = placeInBasis (Frame2d.basis frame) expression

relativeTo ::
  Frame2d (global @ originPointUnits) (Defines local) ->
  Expression Uv.Point (Vector2d (global @ units)) ->
  Expression Uv.Point (Vector2d (local @ units))
relativeTo frame expression = relativeToBasis (Frame2d.basis frame) expression

placeInBasis ::
  Basis2d global (Defines local) ->
  Expression Uv.Point (Vector2d (local @ units)) ->
  Expression Uv.Point (Vector2d (global @ units))
placeInBasis basis vector =
  xComponent vector * Basis2d.xDirection basis + yComponent vector * Basis2d.yDirection basis

relativeToBasis ::
  Basis2d global (Defines local) ->
  Expression Uv.Point (Vector2d (global @ units)) ->
  Expression Uv.Point (Vector2d (local @ units))
relativeToBasis basis vector =
  xy (vector <> Basis2d.xDirection basis) (vector <> Basis2d.yDirection basis)

transformBy ::
  Transform2d a (space @ translationUnits) ->
  Expression Uv.Point (Vector2d (space @ units)) ->
  Expression Uv.Point (Vector2d (space @ units))
transformBy (Transform2d _ i j) vector = xComponent vector * i + yComponent vector * j
