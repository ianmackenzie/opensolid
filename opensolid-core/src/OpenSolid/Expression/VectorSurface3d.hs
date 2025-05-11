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

import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.PlanarBasis3d (PlanarBasis3d)
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
  Basis3d global (Defines local) ->
  Expression UvPoint (Vector3d (local @ units)) ->
  Expression UvPoint (Vector3d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Basis3d global (Defines local) ->
  Expression UvPoint (Vector3d (global @ units)) ->
  Expression UvPoint (Vector3d (local @ units))
relativeTo basis expression =
  rightwardForwardUpward
    # expression `dot` constant (Vector3d.unit (Basis3d.rightwardDirection basis))
    # expression `dot` constant (Vector3d.unit (Basis3d.forwardDirection basis))
    # expression `dot` constant (Vector3d.unit (Basis3d.upwardDirection basis))

projectInto ::
  PlanarBasis3d global (Defines local) ->
  Expression UvPoint (Vector3d (global @ units)) ->
  Expression UvPoint (Vector2d (local @ units))
projectInto = Expression.projectInto

transformBy ::
  Transform3d a (space @ translationUnits) ->
  Expression UvPoint (Vector3d (space @ units)) ->
  Expression UvPoint (Vector3d (space @ units))
transformBy = Expression.transformBy
