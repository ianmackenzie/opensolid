module OpenSolid.Expression.VectorCurve3d
  ( constant
  , on
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , placeIn
  , relativeTo
  , projectInto
  , transformBy
  )
where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Prelude
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector3d (Vector3d)

constant :: Vector3d units space -> Expression Number (Vector3d units space)
constant = Expression.constant

on ::
  Plane3d space local ->
  Expression Number (Vector2d units local) ->
  Expression Number (Vector3d units space)
on = Expression.on

squaredMagnitude_ ::
  Expression Number (Vector3d units space) ->
  Expression Number (Quantity (units ?*? units))
squaredMagnitude_ = Expression.squaredMagnitude_

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression Number (Vector3d units1 space) ->
  Expression Number (Quantity units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression Number (Vector3d units space) -> Expression Number (Quantity units)
magnitude = Expression.magnitude

placeIn ::
  Frame3d global local ->
  Expression Number (Vector3d units local) ->
  Expression Number (Vector3d units global)
placeIn = Expression.placeIn

relativeTo ::
  Frame3d global local ->
  Expression Number (Vector3d units global) ->
  Expression Number (Vector3d units local)
relativeTo = Expression.relativeTo

projectInto ::
  Plane3d global local ->
  Expression Number (Vector3d units global) ->
  Expression Number (Vector2d units local)
projectInto = Expression.projectInto

transformBy ::
  Transform3d a space ->
  Expression Number (Vector3d units space) ->
  Expression Number (Vector3d units space)
transformBy = Expression.transformBy
