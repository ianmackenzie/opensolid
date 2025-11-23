module OpenSolid.Expression.Surface3d
  ( constant
  , on
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
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.UvPoint (UvPoint)

constant :: Point3d space units -> Expression UvPoint (Point3d space units)
constant = Expression.constant

on ::
  Plane3d space units (Defines local) ->
  Expression UvPoint (Point2d local units) ->
  Expression UvPoint (Point3d space units)
on = Expression.on

placeIn ::
  Frame3d global units (Defines local) ->
  Expression UvPoint (Point3d local units) ->
  Expression UvPoint (Point3d global units)
placeIn = Expression.placeIn

relativeTo ::
  Frame3d global units (Defines local) ->
  Expression UvPoint (Point3d global units) ->
  Expression UvPoint (Point3d local units)
relativeTo = Expression.relativeTo

projectInto ::
  Plane3d global units (Defines local) ->
  Expression UvPoint (Point3d global units) ->
  Expression UvPoint (Point2d local units)
projectInto = Expression.projectInto

transformBy ::
  Transform3d tag space units ->
  Expression UvPoint (Point3d space units) ->
  Expression UvPoint (Point3d space units)
transformBy = Expression.transformBy
