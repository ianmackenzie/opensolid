module OpenSolid.Result
  ( Result (Ok, Error)
  , map
  , map2
  , orFail
  , collect
  , foldl
  , foldr
  , sequence
  )
where

import Data.Foldable qualified
import OpenSolid.Prelude
import Prelude qualified

map :: (a -> b) -> Result x a -> Result x b
map = Prelude.fmap

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

foldl :: (b -> a -> Result x b) -> b -> List a -> Result x b
foldl = Data.Foldable.foldlM

foldr :: (a -> b -> Result x b) -> b -> List a -> Result x b
foldr = Data.Foldable.foldrM

sequence :: List (Result x a) -> Result x (List a)
sequence = Prelude.sequence
