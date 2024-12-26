module OpenSolid.Vector3d.CoordinateTransformation
  ( placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  )
where

import {-# SOURCE #-} OpenSolid.Basis3d (Basis3d)
import {-# SOURCE #-} OpenSolid.Basis3d qualified as Basis3d
import {-# SOURCE #-} OpenSolid.Direction3d qualified as Direction3d
import {-# SOURCE #-} OpenSolid.Frame3d (Frame3d)
import {-# SOURCE #-} OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Vector3d (Vector3d)
import {-# SOURCE #-} OpenSolid.Vector3d qualified as Vector3d

placeIn ::
  Frame3d (global @ originUnits) (Defines local) ->
  Vector3d (local @ units) ->
  Vector3d (global @ units)
placeIn frame = placeInBasis (Frame3d.basis frame)

relativeTo ::
  Frame3d (global @ originUnits) (Defines local) ->
  Vector3d (global @ units) ->
  Vector3d (local @ units)
relativeTo frame = relativeToBasis (Frame3d.basis frame)

placeInBasis ::
  Basis3d global (Defines local) ->
  Vector3d (local @ units) ->
  Vector3d (global @ units)
placeInBasis basis vector = do
  let (vx, vy, vz) = Vector3d.components vector
  vx * Basis3d.xDirection basis + vy * Basis3d.yDirection basis + vz * Basis3d.zDirection basis

relativeToBasis ::
  Basis3d global (Defines local) ->
  Vector3d (global @ units) ->
  Vector3d (local @ units)
relativeToBasis basis vector = do
  let (vx, vy, vz) = Vector3d.components vector
  let (ix, iy, iz) = Direction3d.components (Basis3d.xDirection basis)
  let (jx, jy, jz) = Direction3d.components (Basis3d.yDirection basis)
  let (kx, ky, kz) = Direction3d.components (Basis3d.zDirection basis)
  Vector3d.xyz
    (vx * ix + vy * iy + vz * iz)
    (vx * jx + vy * jy + vz * jz)
    (vx * kx + vy * ky + vz * kz)
