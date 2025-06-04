module OpenSolid.Expression.VectorSurface3d
  ( constant
  , rightwardForwardUpward
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
import OpenSolid.Orientation3d (Orientation3d)
import OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.PlaneOrientation3d (PlaneOrientation3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvPoint)
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d

constant :: Vector3d (space @ units) -> Expression UvPoint (Vector3d (space @ units))
constant = Expression.constant

rightwardForwardUpward ::
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Vector3d (space @ units))
rightwardForwardUpward = Expression.rightwardForwardUpward

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
  Orientation3d global (Defines local) ->
  Expression UvPoint (Vector3d (local @ units)) ->
  Expression UvPoint (Vector3d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Orientation3d global (Defines local) ->
  Expression UvPoint (Vector3d (global @ units)) ->
  Expression UvPoint (Vector3d (local @ units))
relativeTo orientation expression =
  rightwardForwardUpward
    @ expression `dot` constant (Vector3d.unit (Orientation3d.rightwardDirection orientation))
    @ expression `dot` constant (Vector3d.unit (Orientation3d.forwardDirection orientation))
    @ expression `dot` constant (Vector3d.unit (Orientation3d.upwardDirection orientation))

projectInto ::
  PlaneOrientation3d global (Defines local) ->
  Expression UvPoint (Vector3d (global @ units)) ->
  Expression UvPoint (Vector2d (local @ units))
projectInto = Expression.projectInto

transformBy ::
  Transform3d a (space @ translationUnits) ->
  Expression UvPoint (Vector3d (space @ units)) ->
  Expression UvPoint (Vector3d (space @ units))
transformBy = Expression.transformBy
