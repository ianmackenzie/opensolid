module Bool (random) where

import Basics
import Random.Internal (Generator (Generator))
import System.Random qualified

random :: Generator Bool
random = Generator System.Random.uniform
