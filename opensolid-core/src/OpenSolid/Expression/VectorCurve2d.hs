module OpenSolid.Expression.VectorCurve2d
  ( constant
  , xy
  , squaredMagnitude
  , squaredMagnitude'
  , magnitude
  , placeIn
  , relativeTo
  , transformBy
  , placeOn
  )
where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector3d (Vector3d)

constant :: Vector2d (space @ units) -> Expression Float (Vector2d (space @ units))
constant = Expression.constant

xy ::
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Vector2d (space @ units))
xy = Expression.xy

squaredMagnitude' ::
  Expression Float (Vector2d (space @ units)) ->
  Expression Float (Qty (units :*: units))
squaredMagnitude' = Expression.squaredMagnitude'

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression Float (Vector2d (space @ units1)) ->
  Expression Float (Qty units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression Float (Vector2d (space @ units)) -> Expression Float (Qty units)
magnitude = Expression.magnitude

placeIn ::
  Frame2d (global @ frameUnits) (Defines local) ->
  Expression Float (Vector2d (local @ units)) ->
  Expression Float (Vector2d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Frame2d (global @ frameUnits) (Defines local) ->
  Expression Float (Vector2d (global @ units)) ->
  Expression Float (Vector2d (local @ units))
relativeTo = Expression.relativeTo

transformBy ::
  Transform2d a (space @ translationUnits) ->
  Expression Float (Vector2d (space @ units)) ->
  Expression Float (Vector2d (space @ units))
transformBy = Expression.transformBy

placeOn ::
  Plane3d (space @ planeUnits) (Defines local) ->
  Expression Float (Vector2d (local @ units)) ->
  Expression Float (Vector3d (space @ units))
placeOn = Expression.on
