module OpenSolid.Point2d.CoordinateTransformation (placeIn, relativeTo) where

import {-# SOURCE #-} OpenSolid.Direction2d qualified as Direction2d
import {-# SOURCE #-} OpenSolid.Frame2d (Frame2d)
import {-# SOURCE #-} OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Point2d (Point2d)
import {-# SOURCE #-} OpenSolid.Point2d qualified as Point2d

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (local @ units) ->
  Point2d (global @ units)
placeIn frame point = do
  let (px, py) = Point2d.coordinates point
  Frame2d.originPoint frame + px * Frame2d.xDirection frame + py * Frame2d.yDirection frame

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (global @ units) ->
  Point2d (local @ units)
relativeTo frame point = do
  let (px, py) = Point2d.coordinates point
  let (x0, y0) = Point2d.coordinates (Frame2d.originPoint frame)
  let dx = px - x0
  let dy = py - y0
  let (ix, iy) = Direction2d.components (Frame2d.xDirection frame)
  let (jx, jy) = Direction2d.components (Frame2d.yDirection frame)
  Point2d.xy (dx * ix + dy * iy) (dx * jx + dy * jy)
