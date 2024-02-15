module Random
  ( Generator
  , init
  , generate
  , step
  , return
  , map
  , map2
  , map3
  , map4
  , bool
  , int
  , float
  , qty
  , list
  , nonEmpty
  , seed
  , pair
  , maybe
  , oneOf
  )
where

import Array qualified
import Maybe qualified
import OpenSolid
import Pair qualified
import System.Random (StdGen)
import System.Random qualified
import System.Random.Stateful qualified
import Task qualified
import Prelude (Applicative, Functor, Monad)

newtype Generator a = Generator (StdGen -> (a, StdGen))

newtype Seed = Seed StdGen

run :: Generator a -> StdGen -> (a, StdGen)
run (Generator generator) stdgen = generator stdgen

instance Functor Generator where fmap = map

instance Applicative Generator where
  pure givenValue = Generator (givenValue,)

  functionGenerator <*> valueGenerator = do
    generatedFunction <- functionGenerator
    generatedValue <- valueGenerator
    return (generatedFunction generatedValue)

instance Monad Generator where
  generatorA >>= function =
    Generator <|
      \stdGen ->
        let (valueA, stdGenA) = run generatorA stdGen
            generatorB = function valueA
         in run generatorB stdGenA

init :: Int -> Seed
init givenSeed = Seed (System.Random.mkStdGen givenSeed)

step :: Generator a -> Seed -> (a, Seed)
step generator (Seed stdGen) =
  let (generatedValue, updatedStdGen) = run generator stdGen
   in (generatedValue, Seed updatedStdGen)

generate :: Generator a -> Task a
generate generator =
  Task.fromIO <|
    System.Random.Stateful.applyAtomicGen (run generator) System.Random.Stateful.globalStdGen

map :: (a -> b) -> Generator a -> Generator b
map function (Generator generator) = Generator (generator >> Pair.mapFirst function)

map2 :: (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 function generatorA generatorB = do
  valueA <- generatorA
  valueB <- generatorB
  return (function valueA valueB)

map3 :: (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 function generatorA generatorB generatorC = do
  valueA <- generatorA
  valueB <- generatorB
  valueC <- generatorC
  return (function valueA valueB valueC)

map4 :: (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 function generatorA generatorB generatorC generatorD = do
  valueA <- generatorA
  valueB <- generatorB
  valueC <- generatorC
  valueD <- generatorD
  return (function valueA valueB valueC valueD)

bool :: Generator Bool
bool = Generator (System.Random.uniformR (False, True))

int :: Int -> Int -> Generator Int
int low high = Generator (System.Random.uniformR (low, high))

float :: Float -> Float -> Generator Float
float = qty

qty :: Qty units -> Qty units -> Generator (Qty units)
qty (Qty low) (Qty high) = map Qty (Generator (System.Random.uniformR (low, high)))

list :: Int -> Generator a -> Generator (List a)
list n _ | n <= 0 = return []
list n itemGenerator = do
  item <- itemGenerator
  rest <- list (n - 1) itemGenerator
  return (item : rest)

nonEmpty :: Int -> Generator a -> Generator (NonEmpty a)
nonEmpty n itemGenerator = do
  first <- itemGenerator
  rest <- list (n - 1) itemGenerator
  return (first :| rest)

seed :: Generator Seed
seed = Generator (System.Random.split >> Pair.mapFirst Seed)

pair :: Generator a -> Generator b -> Generator (a, b)
pair generatorA generatorB = do
  valueA <- generatorA
  valueB <- generatorB
  return (valueA, valueB)

maybe :: Generator a -> Generator (Maybe a)
maybe generator = do
  generateJust <- bool
  if generateJust then map Just generator else return Nothing

oneOf :: Generator a -> List (Generator a) -> Generator a
oneOf firstGenerator remainingGenerators =
  let array = Array.fromList remainingGenerators
      n = Array.length array
      indexGenerator = int 0 n
   in do
        index <- indexGenerator
        Array.get (index - 1) array |> Maybe.withDefault firstGenerator
