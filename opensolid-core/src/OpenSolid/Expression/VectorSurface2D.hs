module OpenSolid.Expression.VectorSurface2D
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
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Units qualified as Units
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector3D (Vector3D)

constant :: Vector2D units space -> Expression UvPoint (Vector2D units space)
constant = Expression.constant

xy ::
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Vector2D units space)
xy = Expression.xy

squaredMagnitude_ ::
  Expression UvPoint (Vector2D units space) ->
  Expression UvPoint (Quantity (units ?*? units))
squaredMagnitude_ = Expression.squaredMagnitude_

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression UvPoint (Vector2D units1 space) ->
  Expression UvPoint (Quantity units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression UvPoint (Vector2D units space) -> Expression UvPoint (Quantity units)
magnitude = Expression.magnitude

placeIn ::
  Frame2D frameUnits global local ->
  Expression UvPoint (Vector2D units local) ->
  Expression UvPoint (Vector2D units global)
placeIn = Expression.placeIn

relativeTo ::
  Frame2D frameUnits global local ->
  Expression UvPoint (Vector2D units global) ->
  Expression UvPoint (Vector2D units local)
relativeTo = Expression.relativeTo

placeOn ::
  Plane3D global local ->
  Expression UvPoint (Vector2D units local) ->
  Expression UvPoint (Vector3D units global)
placeOn = Expression.on

transformBy ::
  Transform2D a translationUnits space ->
  Expression UvPoint (Vector2D units space) ->
  Expression UvPoint (Vector2D units space)
transformBy = Expression.transformBy
