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
  , intFrom
  , floatFrom
  , qtyFrom
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
import Prelude qualified

newtype Generator a = Generator (StdGen -> (a, StdGen))

newtype Seed = Seed StdGen

run :: Generator a -> StdGen -> (a, StdGen)
run (Generator generator) stdgen = generator stdgen

instance Prelude.Functor Generator where fmap = map

instance Prelude.Applicative Generator where
  pure = return
  functionGenerator <*> valueGenerator = do
    generatedFunction <- functionGenerator
    generatedValue <- valueGenerator
    return (generatedFunction generatedValue)

instance Prelude.Monad Generator where (>>=) = Prelude.flip bind

instance a ~ a' => Bind (Generator a) a' (Generator b) where
  bind function generatorA =
    Generator $
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

generate :: Generator a -> Task Text a
generate generator =
  Task.mapError errorMessage $
    Task.fromIO $
      System.Random.Stateful.applyAtomicGen (run generator) System.Random.Stateful.globalStdGen

return :: a -> Generator a
return givenValue = Generator (givenValue,)

map :: (a -> b) -> Generator a -> Generator b
map function generator = Generator (run generator >> Pair.mapFirst function)

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

intFrom :: Int -> Int -> Generator Int
intFrom low high = Generator (System.Random.uniformR (low, high))

floatFrom :: Float -> Float -> Generator Float
floatFrom = qtyFrom

qtyFrom :: Qty units -> Qty units -> Generator (Qty units)
qtyFrom (Qty low) (Qty high) = map Qty (Generator (System.Random.uniformR (low, high)))

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
      indexGenerator = intFrom 0 n
   in do
        index <- indexGenerator
        Array.get (index - 1) array |> Maybe.withDefault firstGenerator
