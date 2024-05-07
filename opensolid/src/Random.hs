module Random
  ( Generator
  , init
  , generate
  , step
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
  , retry
  , (>>=)
  , (>>)
  , return
  )
where

import Array qualified
import Maybe qualified
import OpenSolid
import Pair qualified
import Qty (Qty (Qty_))
import System.Random (StdGen)
import System.Random qualified
import System.Random.Stateful qualified
import Prelude (Applicative, Functor, Monad)
import Prelude qualified

newtype Generator a = Generator (StdGen -> (a, StdGen))

newtype Seed = Seed StdGen

return :: a -> Generator a
return value = Generator (value,)

(>>=) :: Generator a -> (a -> Generator b) -> Generator b
valueGenerator >>= function =
  Generator $
    \stdGen1 -> do
      let (value, stdGen2) = run valueGenerator stdGen1
      let newGenerator = function value
      run newGenerator stdGen2

run :: Generator a -> StdGen -> (a, StdGen)
run (Generator generator) stdgen = generator stdgen

instance Functor Generator where
  fmap = map

instance Applicative Generator where
  pure = return

  functionGenerator <*> valueGenerator =
    Generator $
      \stdGen1 -> do
        let (function, stdGen2) = run functionGenerator stdGen1
        let (value, stdGen3) = run valueGenerator stdGen2
        (function value, stdGen3)

instance Monad Generator where
  (>>=) = (>>=)

init :: Int -> Seed
init givenSeed = Seed (System.Random.mkStdGen givenSeed)

step :: Generator a -> Seed -> (a, Seed)
step generator (Seed stdGen) = Pair.mapSecond Seed (run generator stdGen)

generate :: Generator a -> IO a
generate generator =
  System.Random.Stateful.applyAtomicGen (run generator) System.Random.Stateful.globalStdGen

map :: (a -> b) -> Generator a -> Generator b
map function (Generator generator) = Generator (generator >> Pair.mapFirst function)

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

bool :: Generator Bool
bool = Generator System.Random.uniform

int :: Int -> Int -> Generator Int
int low high = Generator (System.Random.uniformR (low, high))

float :: Float -> Float -> Generator Float
float = qty

qty :: Qty units -> Qty units -> Generator (Qty units)
qty (Qty_ low) (Qty_ high) = map Qty_ (Generator (System.Random.uniformR (low, high)))

list :: Int -> Generator a -> Generator (List a)
list n _ | n <= 0 = return []
list n itemGenerator = Random.do
  item <- itemGenerator
  rest <- list (n - 1) itemGenerator
  return (item : rest)

nonEmpty :: Int -> Generator a -> Generator (NonEmpty a)
nonEmpty n itemGenerator = Random.do
  first <- itemGenerator
  rest <- list (n - 1) itemGenerator
  return (first :| rest)

seed :: Generator Seed
seed = Generator (System.Random.split >> Pair.mapFirst Seed)

pair :: Generator a -> Generator b -> Generator (a, b)
pair generatorA generatorB = Random.do
  valueA <- generatorA
  valueB <- generatorB
  return (valueA, valueB)

maybe :: Generator a -> Generator (Maybe a)
maybe generator = Random.do
  generateJust <- bool
  if generateJust then map Just generator else return Nothing

oneOf :: NonEmpty (Generator a) -> Generator a
oneOf (firstGenerator :| remainingGenerators) = do
  let array = Array.fromList remainingGenerators
  let n = Array.length array
  let indexGenerator = int 0 n
  Random.do
    index <- indexGenerator
    Array.get (index - 1) array |> Maybe.withDefault firstGenerator

retry :: Generator (Result x a) -> Generator a
retry fallibleGenerator = Random.do
  result <- fallibleGenerator
  case result of
    Ok value -> Random.return value
    Error _ -> retry fallibleGenerator
