module OpenSolid.SpurGear
  ( SpurGear
  , new
  , module_
  , numTeeth
  , profile
  , pitchDiameter
  , outerDiameter
  )
where

import OpenSolid.Float qualified as Float
import OpenSolid.Length (Length)
import OpenSolid.Prelude
import OpenSolid.Region2d (Region2d)
import OpenSolid.Units (Meters)

data Space

data SpurGear = SpurGear
  { module_ :: Length
  , numTeeth :: Int
  , profile :: Region2d (Space @ Meters)
  }

new :: Length -> Int -> SpurGear
new givenModule givenNumTeeth = do
  SpurGear
    { module_ = givenModule
    , numTeeth = givenNumTeeth
    , profile = TODO
    }

pitchDiameter :: SpurGear -> Length
pitchDiameter gear = module_ gear * Float.int (numTeeth gear)

outerDiameter :: SpurGear -> Length
outerDiameter gear = pitchDiameter gear + 2.0 * module_ gear
