module OpenSolid.Expression.VectorSurface2d
  ( constant
  , xy
  , squaredMagnitude
  , squaredMagnitude'
  , magnitude
  , placeIn
  , relativeTo
  , on
  , transformBy
  )
where

import OpenSolid.Basis2d (Basis2d)
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.PlanarBasis3d (PlanarBasis3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvPoint)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector3d (Vector3d)

constant :: Vector2d (space @ units) -> Expression UvPoint (Vector2d (space @ units))
constant = Expression.constant

xy ::
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Vector2d (space @ units))
xy = Expression.xy

squaredMagnitude' ::
  Expression UvPoint (Vector2d (space @ units)) ->
  Expression UvPoint (Qty (units :*: units))
squaredMagnitude' = Expression.squaredMagnitude'

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression UvPoint (Vector2d (space @ units1)) ->
  Expression UvPoint (Qty units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression UvPoint (Vector2d (space @ units)) -> Expression UvPoint (Qty units)
magnitude = Expression.magnitude

placeIn ::
  Basis2d global (Defines local) ->
  Expression UvPoint (Vector2d (local @ units)) ->
  Expression UvPoint (Vector2d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Basis2d global (Defines local) ->
  Expression UvPoint (Vector2d (global @ units)) ->
  Expression UvPoint (Vector2d (local @ units))
relativeTo = Expression.relativeTo

on ::
  PlanarBasis3d global (Defines local) ->
  Expression UvPoint (Vector2d (local @ units)) ->
  Expression UvPoint (Vector3d (global @ units))
on = Expression.on

transformBy ::
  Transform2d a (space @ translationUnits) ->
  Expression UvPoint (Vector2d (space @ units)) ->
  Expression UvPoint (Vector2d (space @ units))
transformBy = Expression.transformBy
