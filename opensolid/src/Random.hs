module Random
  ( Generator
  , init
  , generate
  , step
  , map
  , map2
  , map3
  , map4
  , seed
  , either
  , oneOf
  , retry
  , combine
  , (>>=)
  , return
  )
where

import Arithmetic
import Array qualified
import Basics
import Composition
import Data.List.NonEmpty (NonEmpty ((:|)))
import Pair qualified
import Random.Internal hiding ((>>=))
import Random.Internal qualified
import Result (Result (Failure, Success))
import System.Random (StdGen)
import System.Random qualified
import System.Random.Stateful qualified

newtype Seed = Seed StdGen

(>>=) :: Generator a -> (a -> Generator b) -> Generator b
(>>=) = (Random.Internal.>>=)

init :: Int -> Seed
init givenSeed = Seed (System.Random.mkStdGen givenSeed)

step :: Generator a -> Seed -> (a, Seed)
step generator (Seed stdGen) = Pair.mapSecond Seed (run generator stdGen)

generate :: Generator a -> IO a
generate generator =
  System.Random.Stateful.applyAtomicGen (run generator) System.Random.Stateful.globalStdGen

map2 :: (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 function generatorA generatorB = Random.do
  valueA <- generatorA
  valueB <- generatorB
  return (function valueA valueB)

map3 :: (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 function generatorA generatorB generatorC = Random.do
  valueA <- generatorA
  valueB <- generatorB
  valueC <- generatorC
  return (function valueA valueB valueC)

map4 :: (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 function generatorA generatorB generatorC generatorD = Random.do
  valueA <- generatorA
  valueB <- generatorB
  valueC <- generatorC
  valueD <- generatorD
  return (function valueA valueB valueC valueD)

seed :: Generator Seed
seed = Generator (System.Random.split >> Pair.mapFirst Seed)

either :: Generator a -> Generator a -> Generator a
either firstGenerator secondGenerator = Random.do
  coinFlip <- Generator System.Random.uniform
  if coinFlip then firstGenerator else secondGenerator

oneOf :: NonEmpty (Generator a) -> Generator a
oneOf (firstGenerator :| remainingGenerators) = do
  let array = Array.fromList remainingGenerators
  let n = Array.length array
  let indexGenerator = Generator (System.Random.uniformR (0, n))
  Random.do
    index <- indexGenerator
    case Array.get (index - 1) array of
      Just generator -> generator
      Nothing -> firstGenerator

retry :: Generator (Result x a) -> Generator a
retry fallibleGenerator = Random.do
  result <- fallibleGenerator
  case result of
    Success value -> return value
    Failure _ -> retry fallibleGenerator

combine :: List (Generator a) -> Generator (List a)
combine [] = return []
combine (first : rest) = Random.do
  firstValue <- first
  restValues <- combine rest
  return (firstValue : restValues)
