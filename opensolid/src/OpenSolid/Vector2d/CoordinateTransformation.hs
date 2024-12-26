module OpenSolid.Vector2d.CoordinateTransformation
  ( placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  )
where

import {-# SOURCE #-} Basis2d (Basis2d)
import {-# SOURCE #-} Basis2d qualified
import {-# SOURCE #-} Direction2d qualified
import {-# SOURCE #-} Frame2d (Frame2d)
import {-# SOURCE #-} Frame2d qualified
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Vector2d (Vector2d)
import {-# SOURCE #-} OpenSolid.Vector2d qualified as Vector2d

placeIn ::
  Frame2d (global @ originUnits) (Defines local) ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
placeIn frame = placeInBasis (Frame2d.basis frame)

relativeTo ::
  Frame2d (global @ originUnits) (Defines local) ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
relativeTo frame = relativeToBasis (Frame2d.basis frame)

placeInBasis ::
  Basis2d global (Defines local) ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
placeInBasis basis vector = do
  let (vx, vy) = Vector2d.components vector
  vx * Basis2d.xDirection basis + vy * Basis2d.yDirection basis

relativeToBasis ::
  Basis2d global (Defines local) ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
relativeToBasis basis vector = do
  let (vx, vy) = Vector2d.components vector
  let (ix, iy) = Direction2d.components (Basis2d.xDirection basis)
  let (jx, jy) = Direction2d.components (Basis2d.yDirection basis)
  Vector2d.xy (vx * ix + vy * iy) (vx * jx + vy * jy)
