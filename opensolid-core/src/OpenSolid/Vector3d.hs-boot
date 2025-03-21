module OpenSolid.Vector3d
  ( transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Basis3d, Transform3d, Vector3d)

transformBy ::
  Transform3d tag (space @ units1) ->
  Vector3d (space @ units2) ->
  Vector3d (space @ units2)
placeIn ::
  Basis3d global (Defines local) ->
  Vector3d (local @ units) ->
  Vector3d (global @ units)
relativeTo ::
  Basis3d global (Defines local) ->
  Vector3d (global @ units) ->
  Vector3d (local @ units)
