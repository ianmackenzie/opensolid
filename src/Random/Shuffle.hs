module Random.Shuffle (list, nonEmpty) where

import Random (Generator)
import Random qualified
import OpenSolid
import List qualified
import Pair qualified
import NonEmpty qualified
    
list :: Generator (List a) -> Generator (List a)
list generator = do
  original <- generator
  keys <- Random.list (List.length original) (Random.float 0.0 1.0)
  let shuffledPairs = List.sortBy Pair.second (List.zip2 original keys)
  Random.return (List.map Pair.first shuffledPairs)

nonEmpty :: Generator (NonEmpty a) -> Generator (NonEmpty a)
nonEmpty generator = do
  original <- generator
  keys <- Random.nonEmpty (NonEmpty.length original) (Random.float 0.0 1.0)
  let shuffledPairs = NonEmpty.sortBy Pair.second (NonEmpty.zip2 original keys)
  Random.return (NonEmpty.map Pair.first shuffledPairs)
