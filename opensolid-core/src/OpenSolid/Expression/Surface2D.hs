module OpenSolid.Expression.Surface2D
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
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.UvPoint (UvPoint)

constant :: Point2D units space -> Expression UvPoint (Point2D units space)
constant = Expression.constant

xy ::
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Quantity units) ->
  Expression UvPoint (Point2D units space)
xy = Expression.xy

placeIn ::
  Frame2D units global local ->
  Expression UvPoint (Point2D units local) ->
  Expression UvPoint (Point2D units global)
placeIn = Expression.placeIn

relativeTo ::
  Frame2D units global local ->
  Expression UvPoint (Point2D units global) ->
  Expression UvPoint (Point2D units local)
relativeTo = Expression.relativeTo

placeOn ::
  Plane3D global local ->
  Expression UvPoint (Point2D Meters local) ->
  Expression UvPoint (Point3D global)
placeOn = Expression.on

transformBy ::
  Transform2D tag units space ->
  Expression UvPoint (Point2D units space) ->
  Expression UvPoint (Point2D units space)
transformBy = Expression.transformBy
