module Arc2d (Arc2d, placeIn) where

import Frame2d (Frame2d)
import OpenSolid
import Units qualified

type role Arc2d nominal

data Arc2d (coordinateSystem :: CoordinateSystem)

instance
  space1 ~ space2 =>
  Units.Coercion (Arc2d (space1 @ unitsA)) (Arc2d (space2 @ unitsB))

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Arc2d (local @ units) ->
  Arc2d (global @ units)
