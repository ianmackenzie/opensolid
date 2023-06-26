module Tests.Random
  ( length
  , vectorBox3d
  )
where

import Length (Length)
import Length qualified
import OpenSolid
import Random (Generator)
import Random qualified
import Range qualified
import Units (Meters)
import VectorBox3d (VectorBox3d (VectorBox3d))

length :: Generator Length
length = Random.qtyFrom (Length.meters -10.0) (Length.meters 10.0)

vectorBox3d :: Generator (VectorBox3d (space @ Meters))
vectorBox3d = do
  x <- Range.generator length
  y <- Range.generator length
  z <- Range.generator length
  Random.return (VectorBox3d x y z)
