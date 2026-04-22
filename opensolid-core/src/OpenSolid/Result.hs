module OpenSolid.Result
  ( Result (Ok, Error)
  , map
  , andThen
  , map2
  , orFail
  , collect
  , forEach
  , foldl
  , foldr
  , all
  )
where

import Data.Foldable qualified
import OpenSolid.Prelude
import Prelude qualified

map :: (a -> b) -> Result x a -> Result x b
map = Prelude.fmap

andThen :: (a -> Result x b) -> Result x a -> Result x b
andThen function result = result >>= function

map2 :: (a -> b -> c) -> Result x a -> Result x b -> Result x c
map2 function result1 result = do
  value1 <- result1
  value2 <- result
  Ok (function value1 value2)

orFail :: MonadFail m => Result x a -> m a
orFail (Ok value) = Prelude.return value
orFail (Error error) = Prelude.fail (Prelude.show error)

collect :: Traversable list => (a -> Result x b) -> list a -> Result x (list b)
collect = Prelude.mapM

foldl :: Foldable list => (b -> a -> Result x b) -> b -> list a -> Result x b
foldl = Data.Foldable.foldlM

foldr :: Foldable list => (a -> b -> Result x b) -> b -> list a -> Result x b
foldr = Data.Foldable.foldrM

forEach :: Foldable list => list a -> (a -> b -> Result x b) -> b -> Result x b
forEach list function init = foldl (Prelude.flip function) init list

all :: Traversable list => list (Result x a) -> Result x (list a)
all = Prelude.sequence
