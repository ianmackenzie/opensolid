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
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.UvPoint (UvPoint)

constant :: Point3d space -> Expression UvPoint (Point3d space)
constant = Expression.constant

on ::
  Plane3d space local ->
  Expression UvPoint (Point2D Meters local) ->
  Expression UvPoint (Point3d space)
on = Expression.on

placeIn ::
  Frame3d global local ->
  Expression UvPoint (Point3d local) ->
  Expression UvPoint (Point3d global)
placeIn = Expression.placeIn

relativeTo ::
  Frame3d global local ->
  Expression UvPoint (Point3d global) ->
  Expression UvPoint (Point3d local)
relativeTo = Expression.relativeTo

projectInto ::
  Plane3d global local ->
  Expression UvPoint (Point3d global) ->
  Expression UvPoint (Point2D Meters local)
projectInto = Expression.projectInto

transformBy ::
  Transform3d tag space ->
  Expression UvPoint (Point3d space) ->
  Expression UvPoint (Point3d space)
transformBy = Expression.transformBy
