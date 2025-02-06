module OpenSolid.Point3d
  ( origin
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Frame3d, Point3d, Transform3d)

origin :: Point3d (space @ units)
transformBy :: Transform3d tag (space @ units) -> Point3d (space @ units) -> Point3d (space @ units)
placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (local @ units) ->
  Point3d (global @ units)
relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (global @ units) ->
  Point3d (local @ units)
