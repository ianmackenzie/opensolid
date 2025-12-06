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
import Data.Text (Text)
import OpenSolid.List (List)
import {-# SOURCE #-} OpenSolid.Text qualified as Text
import Prelude
  ( Applicative
  , Eq
  , Functor
  , Monad
  , MonadFail
  , Show
  , Traversable
  )
import Prelude qualified

data Result x a where
  Ok :: a -> Result x a
  Error :: Show x => x -> Result x a

{-# COMPLETE Ok, Error #-}

deriving instance (Eq x, Eq a) => Eq (Result x a)

deriving instance (Show x, Show a) => Show (Result x a)

instance Functor (Result x) where
  fmap = map

instance Applicative (Result x) where
  pure = Ok

  Ok function <*> Ok value = Ok (function value)
  Error error <*> _ = Error error
  _ <*> Error error = Error error

instance Monad (Result x) where
  Ok value >>= function = function value
  Error error >>= _ = Error error

instance MonadFail (Result Text) where
  fail message = Error (Text.pack message)

map :: (a -> b) -> Result x a -> Result x b
map function result = case result of
  Ok value -> Ok (function value)
  Error error -> Error error

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
