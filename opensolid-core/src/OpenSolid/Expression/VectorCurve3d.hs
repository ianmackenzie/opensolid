module OpenSolid.Expression.VectorCurve3d
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
import OpenSolid.PlaneOrientation3d (PlaneOrientation3d)
import OpenSolid.Prelude
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector3d (Vector3d)

constant :: Vector3d (space @ units) -> Expression Float (Vector3d (space @ units))
constant = Expression.constant

rightwardForwardUpward ::
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Vector3d (space @ units))
rightwardForwardUpward = Expression.rightwardForwardUpward

squaredMagnitude' ::
  Expression Float (Vector3d (space @ units)) ->
  Expression Float (Qty (units :*: units))
squaredMagnitude' = Expression.squaredMagnitude'

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Expression Float (Vector3d (space @ units1)) ->
  Expression Float (Qty units2)
squaredMagnitude = Expression.squaredMagnitude

magnitude :: Expression Float (Vector3d (space @ units)) -> Expression Float (Qty units)
magnitude = Expression.magnitude

placeIn ::
  Orientation3d global (Defines local) ->
  Expression Float (Vector3d (local @ units)) ->
  Expression Float (Vector3d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Orientation3d global (Defines local) ->
  Expression Float (Vector3d (global @ units)) ->
  Expression Float (Vector3d (local @ units))
relativeTo = Expression.relativeTo

projectInto ::
  PlaneOrientation3d global (Defines local) ->
  Expression Float (Vector3d (global @ units)) ->
  Expression Float (Vector2d (local @ units))
projectInto = Expression.projectInto

transformBy ::
  Transform3d a (space @ translationUnits) ->
  Expression Float (Vector3d (space @ units)) ->
  Expression Float (Vector3d (space @ units))
transformBy = Expression.transformBy
