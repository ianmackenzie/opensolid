module OpenSolid.Expression.Curve2d
  ( constant
  , xy
  , placeIn
  , relativeTo
  , transformBy
  , placeOn
  )
where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d)

constant :: Point2D units space -> Expression Number (Point2D units space)
constant = Expression.constant

xy ::
  Expression Number (Quantity units) ->
  Expression Number (Quantity units) ->
  Expression Number (Point2D units space)
xy = Expression.xy

placeIn ::
  Frame2d units global local ->
  Expression Number (Point2D units local) ->
  Expression Number (Point2D units global)
placeIn = Expression.placeIn

relativeTo ::
  Frame2d units global local ->
  Expression Number (Point2D units global) ->
  Expression Number (Point2D units local)
relativeTo = Expression.relativeTo

placeOn ::
  Plane3d global local ->
  Expression Number (Point2D Meters local) ->
  Expression Number (Point3d global)
placeOn = Expression.on

transformBy ::
  Transform2d tag units space ->
  Expression Number (Point2D units space) ->
  Expression Number (Point2D units space)
transformBy = Expression.transformBy
