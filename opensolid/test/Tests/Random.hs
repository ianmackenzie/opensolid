module Tests.Random
  ( length
  , lengthRange
  , point2d
  , vectorBounds3d
  , bounds2d
  )
where

import Bounds2d (Bounds2d (Bounds2d))
import Length (Length)
import Length qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Random (Generator)
import Random qualified
import Range (Range)
import Range qualified
import Units (Meters)
import VectorBounds3d (VectorBounds3d (VectorBounds3d))

length :: Generator Length
length = Random.qty (Length.meters -10.0) (Length.meters 10.0)

lengthRange :: Generator (Range Meters)
lengthRange = Range.generator length

point2d :: Generator (Point2d (space @ Meters))
point2d = Random.map2 Point2d length length

vectorBounds3d :: Generator (VectorBounds3d (space @ Meters))
vectorBounds3d = Random.map3 VectorBounds3d lengthRange lengthRange lengthRange

bounds2d :: Generator (Bounds2d (space @ Meters))
bounds2d = Random.map2 Bounds2d lengthRange lengthRange
