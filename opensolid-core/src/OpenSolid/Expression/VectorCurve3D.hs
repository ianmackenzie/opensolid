module OpenSolid.Expression.VectorCurve3D
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
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Prelude
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3D (Vector3D)

constant :: Vector3D units space -> Expression Number (Vector3D units space)
constant = Expression.constant

on ::
  Plane3D space local ->
  Expression Number (Vector2D units local) ->
  Expression Number (Vector3D units space)
on = Expression.on

squaredMagnitude_ ::
  Expression Number (Vector3D units space) ->
  Expression Number (Quantity (units ?*? units))
squaredMagnitude_ = Expression.squaredMagnitude_

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression Number (Vector3D units1 space) ->
  Expression Number (Quantity units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression Number (Vector3D units space) -> Expression Number (Quantity units)
magnitude = Expression.magnitude

placeIn ::
  Frame3D global local ->
  Expression Number (Vector3D units local) ->
  Expression Number (Vector3D units global)
placeIn = Expression.placeIn

relativeTo ::
  Frame3D global local ->
  Expression Number (Vector3D units global) ->
  Expression Number (Vector3D units local)
relativeTo = Expression.relativeTo

projectInto ::
  Plane3D global local ->
  Expression Number (Vector3D units global) ->
  Expression Number (Vector2D units local)
projectInto = Expression.projectInto

transformBy ::
  Transform3D a space ->
  Expression Number (Vector3D units space) ->
  Expression Number (Vector3D units space)
transformBy = Expression.transformBy
