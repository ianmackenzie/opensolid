module OpenSolid.Expression.VectorSurface3d
  ( constant
  , squaredMagnitude
  , squaredMagnitude'
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
import OpenSolid.SurfaceParameter (UvPoint)
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector3d (Vector3d)

constant :: Vector3d (space @ units) -> Expression UvPoint (Vector3d (space @ units))
constant = Expression.constant

squaredMagnitude' ::
  Expression UvPoint (Vector3d (space @ units)) ->
  Expression UvPoint (Qty (units :*: units))
squaredMagnitude' = Expression.squaredMagnitude'

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression UvPoint (Vector3d (space @ units1)) ->
  Expression UvPoint (Qty units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression UvPoint (Vector3d (space @ units)) -> Expression UvPoint (Qty units)
magnitude = Expression.magnitude

placeIn ::
  Frame3d (global @ frameUnits) (Defines local) ->
  Expression UvPoint (Vector3d (local @ units)) ->
  Expression UvPoint (Vector3d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Frame3d (global @ frameUnits) (Defines local) ->
  Expression UvPoint (Vector3d (global @ units)) ->
  Expression UvPoint (Vector3d (local @ units))
relativeTo = Expression.relativeTo

projectInto ::
  Plane3d (global @ planeUnits) (Defines local) ->
  Expression UvPoint (Vector3d (global @ units)) ->
  Expression UvPoint (Vector2d (local @ units))
projectInto = Expression.projectInto

transformBy ::
  Transform3d a (space @ translationUnits) ->
  Expression UvPoint (Vector3d (space @ units)) ->
  Expression UvPoint (Vector3d (space @ units))
transformBy = Expression.transformBy
