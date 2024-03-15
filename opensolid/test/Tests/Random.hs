module Tests.Random
  ( length
  , lengthRange
  , point2d
  , vectorBounds3d
  , frame2d
  , bounds2d
  , vectorBounds2d
  )
where

import Bounds2d (Bounds2d (Bounds2d))
import Direction2d qualified
import Frame2d (Frame2d)
import Frame2d qualified
import Length (Length)
import Length qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Random (Generator)
import Random qualified
import Range (Range)
import Range qualified
import Units (Meters)
import VectorBounds2d (VectorBounds2d (VectorBounds2d))
import VectorBounds3d (VectorBounds3d (VectorBounds3d))

length :: Generator Length
length = Random.qty (Length.meters -10.0) (Length.meters 10.0)

lengthRange :: Generator (Range Meters)
lengthRange = Range.generator length

point2d :: Generator (Point2d (space @ Meters))
point2d = Random.map2 Point2d length length

vectorBounds3d :: Generator (VectorBounds3d (space @ Meters))
vectorBounds3d = Random.map3 VectorBounds3d lengthRange lengthRange lengthRange

frame2d :: Generator (Frame2d (global @ Meters) (Defines local))
frame2d = Random.do
  originPoint <- point2d
  direction <- Direction2d.generator
  return (Frame2d.withXDirection direction originPoint)

bounds2d :: Generator (Bounds2d (space @ Meters))
bounds2d = Random.map2 Bounds2d lengthRange lengthRange

vectorBounds2d :: Generator (VectorBounds2d (space @ Meters))
vectorBounds2d = Random.map2 VectorBounds2d lengthRange lengthRange
