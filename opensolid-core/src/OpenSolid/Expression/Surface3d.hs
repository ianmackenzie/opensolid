module OpenSolid.Expression.Surface3d
  ( constant
  , xyz
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
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvPoint)
import OpenSolid.Transform3d (Transform3d)

constant :: Point3d (space @ units) -> Expression UvPoint (Point3d (space @ units))
constant = Expression.constant

xyz ::
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Point3d (space @ units))
xyz = Expression.xyz

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Expression UvPoint (Point3d (local @ units)) ->
  Expression UvPoint (Point3d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Expression UvPoint (Point3d (global @ units)) ->
  Expression UvPoint (Point3d (local @ units))
relativeTo = Expression.relativeTo

projectInto ::
  Plane3d (global @ units) (Defines local) ->
  Expression UvPoint (Point3d (global @ units)) ->
  Expression UvPoint (Point2d (local @ units))
projectInto = Expression.projectInto

transformBy ::
  Transform3d tag (space @ units) ->
  Expression UvPoint (Point3d (space @ units)) ->
  Expression UvPoint (Point3d (space @ units))
transformBy = Expression.transformBy
