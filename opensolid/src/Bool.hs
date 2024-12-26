module Bool (random) where

import OpenSolid.Bootstrap
import OpenSolid.Random.Internal (Generator (Generator))
import System.Random qualified

random :: Generator Bool
random = Generator System.Random.uniform
