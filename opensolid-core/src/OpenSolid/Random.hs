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
  , either
  , oneOf
  , retry
  , combine
  , (>>=)
  , return
  )
where

import Data.List.NonEmpty (NonEmpty)
import OpenSolid.Arithmetic
import OpenSolid.Array qualified as Array
import OpenSolid.Bootstrap
import OpenSolid.Composition
import OpenSolid.Int qualified as Int
import OpenSolid.Pair qualified as Pair
import OpenSolid.Random.Internal hiding ((>>=))
import OpenSolid.Random.Internal qualified as Internal
import OpenSolid.Result (Result (Failure, Success))
import System.Random (StdGen)
import System.Random qualified
import System.Random.Stateful qualified

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
seed = Generator (System.Random.split >> Pair.mapFirst Seed)

either :: Generator a -> Generator a -> Generator a
either firstGenerator secondGenerator = OpenSolid.Random.do
  coinFlip <- Generator System.Random.uniform
  if coinFlip then firstGenerator else secondGenerator

oneOf :: NonEmpty (Generator a) -> Generator a
oneOf generators = do
  let generatorArray = Array.new generators
  let indexGenerator = Int.random 0 (Array.length generatorArray - 1)
  OpenSolid.Random.do
    index <- indexGenerator
    Array.get index generatorArray

retry :: Generator (Result x a) -> Generator a
retry fallibleGenerator = OpenSolid.Random.do
  result <- fallibleGenerator
  case result of
    Success value -> return value
    Failure _ -> retry fallibleGenerator

combine :: List (Generator a) -> Generator (List a)
combine [] = return []
combine (first : rest) = OpenSolid.Random.do
  firstValue <- first
  restValues <- combine rest
  return (firstValue : restValues)
