module OpenSolid.Vector2d
  ( zero
  , coerce
  , normalize
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Frame2d, Transform2d, Vector2d)

zero :: Vector2d (space @ units)
coerce :: Vector2d (space1 @ units1) -> Vector2d (space2 @ units2)
normalize :: Vector2d (space @ units) -> Vector2d (space @ Unitless)
transformBy ::
  Transform2d tag (space @ units1) ->
  Vector2d (space @ units2) ->
  Vector2d (space @ units2)
placeIn ::
  Frame2d (global @ frameUnits) (Defines local) ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
relativeTo ::
  Frame2d (global @ frameUnits) (Defines local) ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
