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

import Axis2d qualified
import Bounds2d (Bounds2d)
import Bounds2d qualified
import Direction2d qualified
import Frame2d (Frame2d)
import Frame2d qualified
import Length (Length)
import Length qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
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
point2d = Random.map2 Point2d.xy length length

vectorBounds3d :: Generator (VectorBounds3d (space @ Meters))
vectorBounds3d = Random.map3 VectorBounds3d lengthRange lengthRange lengthRange

frame2d :: Generator (Frame2d (global @ Meters) (Defines local))
frame2d = Random.do
  originPoint <- point2d
  xDirection <- Direction2d.generator
  let xAxis = Axis2d.through originPoint xDirection
  Random.return (Frame2d.fromXAxis xAxis)

bounds2d :: Generator (Bounds2d (space @ Meters))
bounds2d = Random.map2 Bounds2d.xy lengthRange lengthRange

vectorBounds2d :: Generator (VectorBounds2d (space @ Meters))
vectorBounds2d = Random.map2 VectorBounds2d lengthRange lengthRange
