module Bool (random) where

import Random.Internal (Generator(Generator))
import System.Random qualified
import Basics

random :: Generator Bool
random = Generator System.Random.uniform 
