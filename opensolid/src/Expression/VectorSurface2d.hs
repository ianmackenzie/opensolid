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
import SurfaceParameter (UvPoint)
import Transform2d (Transform2d (Transform2d))
import Units qualified
import Vector2d (Vector2d)
import Vector2d qualified

constant :: Vector2d (space @ units) -> Expression UvPoint (Vector2d (space @ units))
constant = Expression.constant

xy ::
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Vector2d (space @ units))
xy = Expression.xy

xComponent :: Expression UvPoint (Vector2d (space @ units)) -> Expression UvPoint (Qty units)
xComponent = Expression.xComponent

yComponent :: Expression UvPoint (Vector2d (space @ units)) -> Expression UvPoint (Qty units)
yComponent = Expression.yComponent

squaredMagnitude' ::
  Expression UvPoint (Vector2d (space @ units)) ->
  Expression UvPoint (Qty (units :*: units))
squaredMagnitude' vector =
  Expression.squared' (xComponent vector) + Expression.squared' (yComponent vector)

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression UvPoint (Vector2d (space @ units1)) ->
  Expression UvPoint (Qty units2)
squaredMagnitude = Units.specialize . squaredMagnitude'

magnitude :: Expression UvPoint (Vector2d (space @ units)) -> Expression UvPoint (Qty units)
magnitude = Expression.sqrt' . squaredMagnitude'

interpolateFrom ::
  Expression UvPoint (Vector2d (space @ units)) ->
  Expression UvPoint (Vector2d (space @ units)) ->
  Expression UvPoint Float ->
  Expression UvPoint (Vector2d (space @ units))
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (global @ originPointUnits) (Defines local) ->
  Expression UvPoint (Vector2d (local @ units)) ->
  Expression UvPoint (Vector2d (global @ units))
placeIn frame expression = placeInBasis (Frame2d.basis frame) expression

relativeTo ::
  Frame2d (global @ originPointUnits) (Defines local) ->
  Expression UvPoint (Vector2d (global @ units)) ->
  Expression UvPoint (Vector2d (local @ units))
relativeTo frame expression = relativeToBasis (Frame2d.basis frame) expression

placeInBasis ::
  Basis2d global (Defines local) ->
  Expression UvPoint (Vector2d (local @ units)) ->
  Expression UvPoint (Vector2d (global @ units))
placeInBasis basis expression = do
  let i = Vector2d.unit (Basis2d.xDirection basis)
  let j = Vector2d.unit (Basis2d.yDirection basis)
  xComponent expression * constant i + yComponent expression * constant j

relativeToBasis ::
  Basis2d global (Defines local) ->
  Expression UvPoint (Vector2d (global @ units)) ->
  Expression UvPoint (Vector2d (local @ units))
relativeToBasis basis expression = do
  let i = Vector2d.unit (Basis2d.xDirection basis)
  let j = Vector2d.unit (Basis2d.yDirection basis)
  xy (expression <> constant i) (expression <> constant j)

transformBy ::
  Transform2d a (space @ translationUnits) ->
  Expression UvPoint (Vector2d (space @ units)) ->
  Expression UvPoint (Vector2d (space @ units))
transformBy (Transform2d _ i j) vector =
  xComponent vector * constant i + yComponent vector * constant j
