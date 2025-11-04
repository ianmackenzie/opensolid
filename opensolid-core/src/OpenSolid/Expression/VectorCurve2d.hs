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

constant :: Vector2d (space @ units) -> Expression Number (Vector2d (space @ units))
constant = Expression.constant

xy ::
  Expression Number (Quantity units) ->
  Expression Number (Quantity units) ->
  Expression Number (Vector2d (space @ units))
xy = Expression.xy

squaredMagnitude' ::
  Expression Number (Vector2d (space @ units)) ->
  Expression Number (Quantity (units *# units))
squaredMagnitude' = Expression.squaredMagnitude'

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression Number (Vector2d (space @ units1)) ->
  Expression Number (Quantity units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression Number (Vector2d (space @ units)) -> Expression Number (Quantity units)
magnitude = Expression.magnitude

placeIn ::
  Frame2d (global @ frameUnits) (Defines local) ->
  Expression Number (Vector2d (local @ units)) ->
  Expression Number (Vector2d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Frame2d (global @ frameUnits) (Defines local) ->
  Expression Number (Vector2d (global @ units)) ->
  Expression Number (Vector2d (local @ units))
relativeTo = Expression.relativeTo

transformBy ::
  Transform2d a (space @ translationUnits) ->
  Expression Number (Vector2d (space @ units)) ->
  Expression Number (Vector2d (space @ units))
transformBy = Expression.transformBy

placeOn ::
  Plane3d (space @ planeUnits) (Defines local) ->
  Expression Number (Vector2d (local @ units)) ->
  Expression Number (Vector3d (space @ units))
placeOn = Expression.on
