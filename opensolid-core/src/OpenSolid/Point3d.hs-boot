module OpenSolid.Point3d
  ( dummy
  , coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import Data.Void (Void)
import OpenSolid.Prelude
import OpenSolid.Primitives (Frame3d, Point3d, Transform3d)

dummy :: Point3d (space @ Void)
coerce :: Point3d (space1 @ units1) -> Point3d (space2 @ units2)
transformBy :: Transform3d tag (space @ units) -> Point3d (space @ units) -> Point3d (space @ units)
placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (local @ units) ->
  Point3d (global @ units)
relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (global @ units) ->
  Point3d (local @ units)
