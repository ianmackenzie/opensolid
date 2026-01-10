module OpenSolid.Expression.Surface2d
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
import OpenSolid.UvPoint (UvPoint)

constant :: Point2D units space -> Expression UvPoint (Point2D units space)
constant = Expression.constant

xy ::
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Point2D units space)
xy = Expression.xy

placeIn ::
  Frame2d units global local ->
  Expression UvPoint (Point2D units local) ->
  Expression UvPoint (Point2D units global)
placeIn = Expression.placeIn

relativeTo ::
  Frame2d units global local ->
  Expression UvPoint (Point2D units global) ->
  Expression UvPoint (Point2D units local)
relativeTo = Expression.relativeTo

placeOn ::
  Plane3d global local ->
  Expression UvPoint (Point2D Meters local) ->
  Expression UvPoint (Point3d global)
placeOn = Expression.on

transformBy ::
  Transform2d tag units space ->
  Expression UvPoint (Point2D units space) ->
  Expression UvPoint (Point2D units space)
transformBy = Expression.transformBy
