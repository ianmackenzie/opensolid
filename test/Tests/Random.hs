module Tests.Random
  ( length
  , lengthRange
  , point2d
  , vectorBox3d
  )
where

import Length (Length)
import Length qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Random (Generator)
import Random qualified
import Range (Range)
import Range qualified
import Units (Meters)
import VectorBox3d (VectorBox3d (VectorBox3d))

length :: Generator Length
length = Random.qty (Length.meters -10.0) (Length.meters 10.0)

lengthRange :: Generator (Range Meters)
lengthRange = Range.generator length

point2d :: Generator (Point2d (space @ Meters))
point2d = Random.map2 Point2d length length

vectorBox3d :: Generator (VectorBox3d (space @ Meters))
vectorBox3d = Random.map3 VectorBox3d lengthRange lengthRange lengthRange
