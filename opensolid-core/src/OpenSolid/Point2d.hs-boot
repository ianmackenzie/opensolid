module OpenSolid.Point2d
  ( origin
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Frame2d, Point2d, Transform2d)

origin :: Point2d (space @ units)
transformBy :: Transform2d tag (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (local @ units) ->
  Point2d (global @ units)
relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (global @ units) ->
  Point2d (local @ units)
