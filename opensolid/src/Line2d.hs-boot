module Line2d (Line2d, placeIn) where

import Frame2d (Frame2d)
import OpenSolid
import Units qualified

type role Line2d phantom

data Line2d (coordinateSystem :: CoordinateSystem)

instance
  space1 ~ space2 =>
  Units.Coercion (Line2d (space1 @ unitsA)) (Line2d (space2 @ unitsB))

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Line2d (local @ units) ->
  Line2d (global @ units)
