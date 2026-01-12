module OpenSolid.Expression.VectorCurve2D
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
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector3D (Vector3D)

constant :: Vector2D units space -> Expression Number (Vector2D units space)
constant = Expression.constant

xy ::
  Expression Number (Quantity units) ->
  Expression Number (Quantity units) ->
  Expression Number (Vector2D units space)
xy = Expression.xy

squaredMagnitude_ ::
  Expression Number (Vector2D units space) ->
  Expression Number (Quantity (units ?*? units))
squaredMagnitude_ = Expression.squaredMagnitude_

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression Number (Vector2D units1 space) ->
  Expression Number (Quantity units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression Number (Vector2D units space) -> Expression Number (Quantity units)
magnitude = Expression.magnitude

placeIn ::
  Frame2D frameUnits global local ->
  Expression Number (Vector2D units local) ->
  Expression Number (Vector2D units global)
placeIn = Expression.placeIn

relativeTo ::
  Frame2D frameUnits global local ->
  Expression Number (Vector2D units global) ->
  Expression Number (Vector2D units local)
relativeTo = Expression.relativeTo

transformBy ::
  Transform2D a translationUnits space ->
  Expression Number (Vector2D units space) ->
  Expression Number (Vector2D units space)
transformBy = Expression.transformBy

placeOn ::
  Plane3D space local ->
  Expression Number (Vector2D units local) ->
  Expression Number (Vector3D units space)
placeOn = Expression.on
