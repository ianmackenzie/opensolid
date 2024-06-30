module Point3d.CoordinateTransformation (placeIn, relativeTo) where

import {-# SOURCE #-} Direction3d qualified
import {-# SOURCE #-} Frame3d (Frame3d)
import {-# SOURCE #-} Frame3d qualified
import OpenSolid
import {-# SOURCE #-} Point3d (Point3d)
import {-# SOURCE #-} Point3d qualified

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (local @ units) ->
  Point3d (global @ units)
placeIn frame point = do
  let (px, py, pz) = Point3d.coordinates point
  Frame3d.originPoint frame
    + px * Frame3d.xDirection frame
    + py * Frame3d.yDirection frame
    + pz * Frame3d.zDirection frame

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Point3d (global @ units) ->
  Point3d (local @ units)
relativeTo frame point = do
  let (px, py, pz) = Point3d.coordinates point
  let (x0, y0, z0) = Point3d.coordinates (Frame3d.originPoint frame)
  let dx = px - x0
  let dy = py - y0
  let dz = pz - z0
  let (ix, iy, iz) = Direction3d.components (Frame3d.xDirection frame)
  let (jx, jy, jz) = Direction3d.components (Frame3d.yDirection frame)
  let (kx, ky, kz) = Direction3d.components (Frame3d.zDirection frame)
  Point3d.xyz
    (dx * ix + dy * iy + dz * iz)
    (dx * jx + dy * jy + dz * jz)
    (dx * kx + dy * ky + dz * kz)
