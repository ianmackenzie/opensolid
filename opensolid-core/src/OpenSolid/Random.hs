module OpenSolid.Random
  ( Generator
  , Seed
  , init
  , generate
  , step
  , map
  , map2
  , map3
  , map4
  , seed
  , oneOf
  , merge
  , retry
  , filter
  , sequence
  , (>>=)
  , return
  )
where

import Data.List.NonEmpty (NonEmpty)
import OpenSolid.Arithmetic
import OpenSolid.Array qualified as Array
import OpenSolid.Bootstrap hiding (sequence)
import OpenSolid.Composition
import OpenSolid.Int qualified as Int
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Random.Internal hiding ((>>=))
import OpenSolid.Random.Internal qualified as Internal
import System.Random (StdGen)
import System.Random qualified
import System.Random.Stateful qualified
import Prelude qualified

newtype Seed = Seed StdGen

(>>=) :: Generator a -> (a -> Generator b) -> Generator b
(>>=) = (Internal.>>=)

infixl 1 >>=

init :: Int -> Seed
init givenSeed = Seed (System.Random.mkStdGen givenSeed)

step :: Generator a -> Seed -> (a, Seed)
step generator (Seed stdGen) = Pair.mapSecond Seed (run generator stdGen)

generate :: Generator a -> IO a
generate generator =
  System.Random.Stateful.applyAtomicGen (run generator) System.Random.Stateful.globalStdGen

map2 :: (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 function generatorA generatorB = OpenSolid.Random.do
  valueA <- generatorA
  valueB <- generatorB
  return (function valueA valueB)

map3 :: (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 function generatorA generatorB generatorC = OpenSolid.Random.do
  valueA <- generatorA
  valueB <- generatorB
  valueC <- generatorC
  return (function valueA valueB valueC)

map4 :: (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 function generatorA generatorB generatorC generatorD = OpenSolid.Random.do
  valueA <- generatorA
  valueB <- generatorB
  valueC <- generatorC
  valueD <- generatorD
  return (function valueA valueB valueC valueD)

seed :: Generator Seed
seed = Generator (System.Random.splitGen >> Pair.mapFirst Seed)

oneOf :: NonEmpty a -> Generator a
oneOf values = merge (NonEmpty.map return values)

merge :: NonEmpty (Generator a) -> Generator a
merge generators = do
  let generatorArray = Array.fromNonEmpty generators
  let indexGenerator = Int.random 0 (generatorArray.length - 1)
  OpenSolid.Random.do
    index <- indexGenerator
    Array.get index generatorArray

retry :: Generator (Maybe a) -> Generator a
retry fallibleGenerator = OpenSolid.Random.do
  result <- fallibleGenerator
  case result of
    Just value -> return value
    Nothing -> retry fallibleGenerator

filter :: (a -> Bool) -> Generator a -> Generator a
filter predicate generator = OpenSolid.Random.do
  result <- generator
  if predicate result
    then return result
    else filter predicate generator

sequence :: Traversable list => list (Generator a) -> Generator (list a)
sequence = Prelude.sequence
