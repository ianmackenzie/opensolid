module OpenSolid.Random.Internal where

import OpenSolid.Bootstrap
import OpenSolid.Composition
import OpenSolid.Pair qualified as Pair
import System.Random (StdGen)
import Prelude qualified

newtype Generator a = Generator (StdGen -> (a, StdGen))

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

infixl 1 >>=

run :: Generator a -> StdGen -> (a, StdGen)
run (Generator generator) stdgen = generator stdgen

map :: (a -> b) -> Generator a -> Generator b
map function (Generator generator) = Generator (generator >> Pair.mapFirst function)

return :: a -> Generator a
return value = Generator (value,)

(>>=) :: Generator a -> (a -> Generator b) -> Generator b
valueGenerator >>= function =
  Generator $
    \stdGen1 -> do
      let (value, stdGen2) = run valueGenerator stdGen1
      let newGenerator = function value
      run newGenerator stdGen2
