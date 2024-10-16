module Function.VectorCurve2d
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
import Function (Function)
import Function qualified
import Frame2d (Frame2d)
import Frame2d qualified
import OpenSolid
import Transform2d (Transform2d (Transform2d))
import Units qualified
import Vector2d (Vector2d)

constant :: Vector2d (space @ units) -> Function Float (Vector2d (space @ units))
constant = Function.constant

xy ::
  Function Float (Qty units) ->
  Function Float (Qty units) ->
  Function Float (Vector2d (space @ units))
xy = Function.xy

xComponent :: Function Float (Vector2d (space @ units)) -> Function Float (Qty units)
xComponent = Function.xComponent

yComponent :: Function Float (Vector2d (space @ units)) -> Function Float (Qty units)
yComponent = Function.yComponent

squaredMagnitude' ::
  Function Float (Vector2d (space @ units)) ->
  Function Float (Qty (units :*: units))
squaredMagnitude' vector =
  Function.squared' (xComponent vector) + Function.squared' (yComponent vector)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Function Float (Vector2d (space @ units1)) ->
  Function Float (Qty units2)
squaredMagnitude = Units.specialize . squaredMagnitude'

magnitude :: Function Float (Vector2d (space @ units)) -> Function Float (Qty units)
magnitude = Function.sqrt' . squaredMagnitude'

interpolateFrom ::
  Function Float (Vector2d (space @ units)) ->
  Function Float (Vector2d (space @ units)) ->
  Function Float Float ->
  Function Float (Vector2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (global @ originPointUnits) (Defines local) ->
  Function Float (Vector2d (local @ units)) ->
  Function Float (Vector2d (global @ units))
placeIn frame expression = placeInBasis (Frame2d.basis frame) expression

relativeTo ::
  Frame2d (global @ originPointUnits) (Defines local) ->
  Function Float (Vector2d (global @ units)) ->
  Function Float (Vector2d (local @ units))
relativeTo frame expression = relativeToBasis (Frame2d.basis frame) expression

placeInBasis ::
  Basis2d global (Defines local) ->
  Function Float (Vector2d (local @ units)) ->
  Function Float (Vector2d (global @ units))
placeInBasis basis vector =
  xComponent vector * Basis2d.xDirection basis + yComponent vector * Basis2d.yDirection basis

relativeToBasis ::
  Basis2d global (Defines local) ->
  Function Float (Vector2d (global @ units)) ->
  Function Float (Vector2d (local @ units))
relativeToBasis basis vector =
  xy (vector <> Basis2d.xDirection basis) (vector <> Basis2d.yDirection basis)

transformBy ::
  Transform2d a (space @ translationUnits) ->
  Function Float (Vector2d (space @ units)) ->
  Function Float (Vector2d (space @ units))
transformBy (Transform2d _ i j) vector = xComponent vector * i + yComponent vector * j
