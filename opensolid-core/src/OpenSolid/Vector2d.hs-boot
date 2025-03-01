module OpenSolid.Vector2d
  ( zero
  , normalize
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Basis2d, Transform2d, Vector2d)

zero :: Vector2d (space @ units)
normalize :: Vector2d (space @ units) -> Vector2d (space @ Unitless)
transformBy :: Transform2d tag (space @ units1) -> Vector2d (space @ units2) -> Vector2d (space @ units2)
placeIn ::
  Basis2d global (Defines local) ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
relativeTo ::
  Basis2d global (Defines local) ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
