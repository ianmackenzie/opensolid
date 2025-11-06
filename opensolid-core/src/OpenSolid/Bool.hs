module OpenSolid.Bool (random) where

import OpenSolid.Random.Internal (Generator (Generator))
import System.Random qualified
import Prelude (Bool)

random :: Generator Bool
random = Generator System.Random.uniform
