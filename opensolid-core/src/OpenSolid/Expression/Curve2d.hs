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
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d)

constant :: Point2d (space @ units) -> Expression Float (Point2d (space @ units))
constant = Expression.constant

xy ::
  Expression Float (Quantity units) ->
  Expression Float (Quantity units) ->
  Expression Float (Point2d (space @ units))
xy = Expression.xy

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Expression Float (Point2d (local @ units)) ->
  Expression Float (Point2d (global @ units))
placeIn = Expression.placeIn

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Expression Float (Point2d (global @ units)) ->
  Expression Float (Point2d (local @ units))
relativeTo = Expression.relativeTo

placeOn ::
  Plane3d (global @ units) (Defines local) ->
  Expression Float (Point2d (local @ units)) ->
  Expression Float (Point3d (global @ units))
placeOn = Expression.on

transformBy ::
  Transform2d tag (space @ units) ->
  Expression Float (Point2d (space @ units)) ->
  Expression Float (Point2d (space @ units))
transformBy = Expression.transformBy
