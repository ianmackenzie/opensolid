module Random
  ( Generator
  , init
  , generate
  , step
  , return
  , map
  , bool
  , intFrom
  , floatFrom
  , qtyFrom
  , list
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

generate :: Generator a -> Task IOError a
generate generator =
  Task.fromIO $
    System.Random.Stateful.applyAtomicGen (run generator) System.Random.Stateful.globalStdGen

return :: a -> Generator a
return givenValue = Generator (givenValue,)

map :: (a -> b) -> Generator a -> Generator b
map function generator = Generator (run generator >> Pair.mapFirst function)

bool :: Generator Bool
bool = Generator (System.Random.uniformR (False, True))

intFrom :: Int -> Int -> Generator Int
intFrom low high = Generator (System.Random.uniformR (low, high))

floatFrom :: Float -> Float -> Generator Float
floatFrom = qtyFrom

qtyFrom :: Qty units -> Qty units -> Generator (Qty units)
qtyFrom (Qty low) (Qty high) = map Qty (Generator (System.Random.uniformR (low, high)))

list :: Int -> Generator a -> Generator (List a)
list 0 _ = return []
list n itemGenerator = do
  item <- itemGenerator
  rest <- list (n - 1) itemGenerator
  return (item : rest)

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
