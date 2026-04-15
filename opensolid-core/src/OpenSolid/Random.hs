module OpenSolid.Random
  ( Generator
  , Seed
  , init
  , generate
  , step
  , int
  , number
  , quantity
  , bool
  , sign
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
  , collect
  , return
  )
where

import Data.List.NonEmpty qualified
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import System.Random (StdGen)
import System.Random qualified
import System.Random.Stateful qualified
import Prelude qualified

newtype Generator a = Generator (StdGen -> (a, StdGen))

instance Functor Generator where
  fmap = map

instance Applicative Generator where
  pure value = Generator (value,)

  functionGenerator <*> valueGenerator =
    Generator \stdGen1 -> do
      let (function, stdGen2) = run functionGenerator stdGen1
      let (value, stdGen3) = run valueGenerator stdGen2
      (function value, stdGen3)

instance Monad Generator where
  valueGenerator >>= function =
    Generator \stdGen1 -> do
      let (value, stdGen2) = run valueGenerator stdGen1
      let newGenerator = function value
      run newGenerator stdGen2

return :: a -> Generator a
return value = Generator (value,)

run :: Generator a -> StdGen -> (a, StdGen)
run (Generator generator) stdgen = generator stdgen

map :: (a -> b) -> Generator a -> Generator b
map function (Generator generator) = Generator (Pair.mapFirst function . generator)

newtype Seed = Seed StdGen

init :: Int -> Seed
init givenSeed = Seed (System.Random.mkStdGen givenSeed)

step :: Generator a -> Seed -> (a, Seed)
step generator (Seed stdGen) = Pair.mapSecond Seed (run generator stdGen)

int :: Int -> Int -> Generator Int
int low high = Generator (System.Random.uniformR (low, high))

number :: Number -> Number -> Generator Number
number = quantity

quantity :: Quantity units -> Quantity units -> Generator (Quantity units)
quantity (Quantity low) (Quantity high) =
  map Quantity (Generator (System.Random.uniformR (low, high)))

bool :: Generator Bool
bool = Generator System.Random.uniform

sign :: Generator Sign
sign = Generator System.Random.uniform

generate :: Generator a -> IO a
generate generator =
  System.Random.Stateful.applyAtomicGen (run generator) System.Random.Stateful.globalStdGen

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

map4 ::
  (a -> b -> c -> d -> e) ->
  Generator a ->
  Generator b ->
  Generator c ->
  Generator d ->
  Generator e
map4 function generatorA generatorB generatorC generatorD = do
  valueA <- generatorA
  valueB <- generatorB
  valueC <- generatorC
  valueD <- generatorD
  return (function valueA valueB valueC valueD)

seed :: Generator Seed
seed = Generator (Pair.mapFirst Seed . System.Random.splitGen)

oneOf :: NonEmpty a -> Generator a
oneOf values = merge (Data.List.NonEmpty.map return values)

merge :: NonEmpty (Generator a) -> Generator a
merge generators = do
  let n = Data.List.NonEmpty.length generators
  index <- int 0 (n - 1)
  generators !! index

retry :: Generator (Maybe a) -> Generator a
retry fallibleGenerator = do
  result <- fallibleGenerator
  case result of
    Just value -> return value
    Nothing -> retry fallibleGenerator

filter :: (a -> Bool) -> Generator a -> Generator a
filter predicate generator = do
  result <- generator
  if predicate result
    then return result
    else filter predicate generator

sequence :: Traversable list => list (Generator a) -> Generator (list a)
sequence = Prelude.sequence

collect :: Traversable list => (a -> Generator b) -> list a -> Generator (list b)
collect = Prelude.mapM
