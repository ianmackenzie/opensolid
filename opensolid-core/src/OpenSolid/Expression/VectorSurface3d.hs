module OpenSolid.Expression.VectorSurface3d
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
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector3d (Vector3d)

constant :: Vector3d units space -> Expression UvPoint (Vector3d units space)
constant = Expression.constant

on ::
  Plane3d space local ->
  Expression UvPoint (Vector2D units local) ->
  Expression UvPoint (Vector3d units space)
on = Expression.on

squaredMagnitude_ ::
  Expression UvPoint (Vector3d units space) ->
  Expression UvPoint (Quantity (units ?*? units))
squaredMagnitude_ = Expression.squaredMagnitude_

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression UvPoint (Vector3d units1 space) ->
  Expression UvPoint (Quantity units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression UvPoint (Vector3d units space) -> Expression UvPoint (Quantity units)
magnitude = Expression.magnitude

placeIn ::
  Frame3d global local ->
  Expression UvPoint (Vector3d units local) ->
  Expression UvPoint (Vector3d units global)
placeIn = Expression.placeIn

relativeTo ::
  Frame3d global local ->
  Expression UvPoint (Vector3d units global) ->
  Expression UvPoint (Vector3d units local)
relativeTo = Expression.relativeTo

projectInto ::
  Plane3d global local ->
  Expression UvPoint (Vector3d units global) ->
  Expression UvPoint (Vector2D units local)
projectInto = Expression.projectInto

transformBy ::
  Transform3d a space ->
  Expression UvPoint (Vector3d units space) ->
  Expression UvPoint (Vector3d units space)
transformBy = Expression.transformBy
