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
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvPoint)
import OpenSolid.Transform2d (Transform2d)

constant :: Point2d (space @ units) -> Expression UvPoint (Point2d (space @ units))
constant = Expression.constant

xy ::
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Qty units) ->
  Expression UvPoint (Point2d (space @ units))
xy = Expression.xy

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Expression UvPoint (Point2d (local @ units)) ->
  Expression UvPoint (Point2d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Expression UvPoint (Point2d (global @ units)) ->
  Expression UvPoint (Point2d (local @ units))
relativeTo = Expression.relativeTo

placeOn ::
  Plane3d (global @ units) (Defines local) ->
  Expression UvPoint (Point2d (local @ units)) ->
  Expression UvPoint (Point3d (global @ units))
placeOn = Expression.placeOn

transformBy ::
  Transform2d tag (space @ units) ->
  Expression UvPoint (Point2d (space @ units)) ->
  Expression UvPoint (Point2d (space @ units))
transformBy = Expression.transformBy
