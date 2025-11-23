module OpenSolid.Expression.VectorSurface2d
  ( constant
  , xy
  , squaredMagnitude
  , squaredMagnitude#
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

constant :: Vector2d space units -> Expression UvPoint (Vector2d space units)
constant = Expression.constant

xy ::
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Vector2d space units)
xy = Expression.xy

squaredMagnitude# ::
  Expression UvPoint (Vector2d space units) ->
  Expression UvPoint (Quantity (units #*# units))
squaredMagnitude# = Expression.squaredMagnitude#

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression UvPoint (Vector2d space units1) ->
  Expression UvPoint (Quantity units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression UvPoint (Vector2d space units) -> Expression UvPoint (Quantity units)
magnitude = Expression.magnitude

placeIn ::
  Frame2d global frameUnits (Defines local) ->
  Expression UvPoint (Vector2d local units) ->
  Expression UvPoint (Vector2d global units)
placeIn = Expression.placeIn

relativeTo ::
  Frame2d global frameUnits (Defines local) ->
  Expression UvPoint (Vector2d global units) ->
  Expression UvPoint (Vector2d local units)
relativeTo = Expression.relativeTo

placeOn ::
  Plane3d global planeUnits (Defines local) ->
  Expression UvPoint (Vector2d local units) ->
  Expression UvPoint (Vector3d global units)
placeOn = Expression.on

transformBy ::
  Transform2d a space translationUnits ->
  Expression UvPoint (Vector2d space units) ->
  Expression UvPoint (Vector2d space units)
transformBy = Expression.transformBy
