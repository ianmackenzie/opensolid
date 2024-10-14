module Expression.Vector2d
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
import Expression.Expression1d qualified as Expression1d
import Frame2d (Frame2d)
import Frame2d qualified
import OpenSolid
import Transform2d (Transform2d (Transform2d))
import Units qualified
import Vector2d (Vector2d (Vector2d))

constant :: Vector2d (space @ units) -> Expression input (Vector2d (space @ units))
constant (Vector2d vx vy) =
  Expression.Vector2d
    (Expression1d.constant (Units.coerce vx))
    (Expression1d.constant (Units.coerce vy))

xy ::
  Expression input (Qty units) ->
  Expression input (Qty units) ->
  Expression input (Vector2d (space @ units))
xy (Expression.Qty x) (Expression.Qty y) = Expression.Vector2d x y

xComponent :: Expression input (Vector2d (space @ units)) -> Expression input (Qty units)
xComponent (Expression.Vector2d vx _) = Expression.Qty vx

yComponent :: Expression input (Vector2d (space @ units)) -> Expression input (Qty units)
yComponent (Expression.Vector2d _ vy) = Expression.Qty vy

squaredMagnitude' ::
  Expression input (Vector2d (space @ units)) ->
  Expression input (Qty (units :*: units))
squaredMagnitude' vector =
  Expression.squared' (xComponent vector) + Expression.squared' (yComponent vector)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression input (Vector2d (space @ units1)) ->
  Expression input (Qty units2)
squaredMagnitude = Units.specialize . squaredMagnitude'

magnitude :: Expression input (Vector2d (space @ units)) -> Expression input (Qty units)
magnitude = Expression.sqrt' . squaredMagnitude'

interpolateFrom ::
  Expression input (Vector2d (space @ units)) ->
  Expression input (Vector2d (space @ units)) ->
  Expression input Float ->
  Expression input (Vector2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (global @ originPointUnits) (Defines local) ->
  Expression input (Vector2d (local @ units)) ->
  Expression input (Vector2d (global @ units))
placeIn frame expression = placeInBasis (Frame2d.basis frame) expression

relativeTo ::
  Frame2d (global @ originPointUnits) (Defines local) ->
  Expression input (Vector2d (global @ units)) ->
  Expression input (Vector2d (local @ units))
relativeTo frame expression = relativeToBasis (Frame2d.basis frame) expression

placeInBasis ::
  Basis2d global (Defines local) ->
  Expression input (Vector2d (local @ units)) ->
  Expression input (Vector2d (global @ units))
placeInBasis basis vector =
  xComponent vector * Basis2d.xDirection basis + yComponent vector * Basis2d.yDirection basis

relativeToBasis ::
  Basis2d global (Defines local) ->
  Expression input (Vector2d (global @ units)) ->
  Expression input (Vector2d (local @ units))
relativeToBasis basis vector =
  xy (vector <> Basis2d.xDirection basis) (vector <> Basis2d.yDirection basis)

transformBy ::
  Transform2d a (space @ translationUnits) ->
  Expression input (Vector2d (space @ units)) ->
  Expression input (Vector2d (space @ units))
transformBy (Transform2d _ i j) vector = xComponent vector * i + yComponent vector * j
