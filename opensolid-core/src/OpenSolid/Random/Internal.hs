module OpenSolid.Random.Internal where

import OpenSolid.Bootstrap
import OpenSolid.Pair qualified as Pair
import System.Random (StdGen)

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

run :: Generator a -> StdGen -> (a, StdGen)
run (Generator generator) stdgen = generator stdgen

map :: (a -> b) -> Generator a -> Generator b
map function (Generator generator) = Generator (Pair.mapFirst function . generator)
