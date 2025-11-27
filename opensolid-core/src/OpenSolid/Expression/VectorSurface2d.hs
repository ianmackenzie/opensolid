module OpenSolid.Expression.VectorSurface2d
  ( constant
  , xy
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , placeIn
  , relativeTo
  , placeOn
  , transformBy
  )
where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector3d (Vector3d)

constant :: Vector2d units space -> Expression UvPoint (Vector2d units space)
constant = Expression.constant

xy ::
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Vector2d units space)
xy = Expression.xy

squaredMagnitude_ ::
  Expression UvPoint (Vector2d units space) ->
  Expression UvPoint (Quantity (units ?*? units))
squaredMagnitude_ = Expression.squaredMagnitude_

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression UvPoint (Vector2d units1 space) ->
  Expression UvPoint (Quantity units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression UvPoint (Vector2d units space) -> Expression UvPoint (Quantity units)
magnitude = Expression.magnitude

placeIn ::
  Frame2d frameUnits global local ->
  Expression UvPoint (Vector2d units local) ->
  Expression UvPoint (Vector2d units global)
placeIn = Expression.placeIn

relativeTo ::
  Frame2d frameUnits global local ->
  Expression UvPoint (Vector2d units global) ->
  Expression UvPoint (Vector2d units local)
relativeTo = Expression.relativeTo

placeOn ::
  Plane3d global local ->
  Expression UvPoint (Vector2d units local) ->
  Expression UvPoint (Vector3d units global)
placeOn = Expression.on

transformBy ::
  Transform2d a translationUnits space ->
  Expression UvPoint (Vector2d units space) ->
  Expression UvPoint (Vector2d units space)
transformBy = Expression.transformBy
