module OpenSolid.Expression.VectorCurve2d
  ( constant
  , xy
  , squaredMagnitude
  , squaredMagnitude_
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

constant :: Vector2d units space -> Expression Number (Vector2d units space)
constant = Expression.constant

xy ::
  Expression Number (Quantity units) ->
  Expression Number (Quantity units) ->
  Expression Number (Vector2d units space)
xy = Expression.xy

squaredMagnitude_ ::
  Expression Number (Vector2d units space) ->
  Expression Number (Quantity (units ?*? units))
squaredMagnitude_ = Expression.squaredMagnitude_

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression Number (Vector2d units1 space) ->
  Expression Number (Quantity units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression Number (Vector2d units space) -> Expression Number (Quantity units)
magnitude = Expression.magnitude

placeIn ::
  Frame2d frameUnits global local ->
  Expression Number (Vector2d units local) ->
  Expression Number (Vector2d units global)
placeIn = Expression.placeIn

relativeTo ::
  Frame2d frameUnits global local ->
  Expression Number (Vector2d units global) ->
  Expression Number (Vector2d units local)
relativeTo = Expression.relativeTo

transformBy ::
  Transform2d a translationUnits space ->
  Expression Number (Vector2d units space) ->
  Expression Number (Vector2d units space)
transformBy = Expression.transformBy

placeOn ::
  Plane3d space local ->
  Expression Number (Vector2d units local) ->
  Expression Number (Vector3d units space)
placeOn = Expression.on
