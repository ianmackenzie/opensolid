module OpenSolid.Expression.Curve3D
  ( constant
  , on
  , placeIn
  , relativeTo
  , transformBy
  , projectInto
  )
where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Transform3D (Transform3D)

constant :: Point3D space -> Expression Number (Point3D space)
constant = Expression.constant

on ::
  Plane3D space local ->
  Expression Number (Point2D Meters local) ->
  Expression Number (Point3D space)
on = Expression.on

placeIn ::
  Frame3D global local ->
  Expression Number (Point3D local) ->
  Expression Number (Point3D global)
placeIn = Expression.placeIn

relativeTo ::
  Frame3D global local ->
  Expression Number (Point3D global) ->
  Expression Number (Point3D local)
relativeTo = Expression.relativeTo

projectInto ::
  Plane3D global local ->
  Expression Number (Point3D global) ->
  Expression Number (Point2D Meters local)
projectInto = Expression.projectInto

transformBy ::
  Transform3D tag space ->
  Expression Number (Point3D space) ->
  Expression Number (Point3D space)
transformBy = Expression.transformBy
