module OpenSolid.Expression.Curve3d
  ( constant
  , xyz
  , placeIn
  , relativeTo
  , transformBy
  , projectInto
  )
where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Transform3d (Transform3d)

constant :: Point3d (space @ units) -> Expression Float (Point3d (space @ units))
constant = Expression.constant

xyz ::
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Qty units) ->
  Expression Float (Point3d (space @ units))
xyz = Expression.xyz

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Expression Float (Point3d (local @ units)) ->
  Expression Float (Point3d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Expression Float (Point3d (global @ units)) ->
  Expression Float (Point3d (local @ units))
relativeTo = Expression.relativeTo

projectInto ::
  Plane3d (global @ units) (Defines local) ->
  Expression Float (Point3d (global @ units)) ->
  Expression Float (Point2d (local @ units))
projectInto = Expression.projectInto

transformBy ::
  Transform3d tag (space @ units) ->
  Expression Float (Point3d (space @ units)) ->
  Expression Float (Point3d (space @ units))
transformBy = Expression.transformBy
