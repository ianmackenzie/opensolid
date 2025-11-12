module OpenSolid.Result
  ( Result (Success, Failure)
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
import OpenSolid.Error qualified as Error
import OpenSolid.List (List)
import {-# SOURCE #-} OpenSolid.Text qualified as Text
import Prelude
  ( Applicative
  , Eq
  , Foldable
  , Functor
  , Monad
  , MonadFail
  , Show
  , Traversable
  )
import Prelude qualified

data Result x a where
  Success :: a -> Result x a
  Failure :: Error.Message x => x -> Result x a

{-# COMPLETE Success, Failure #-}

deriving instance (Eq x, Eq a) => Eq (Result x a)

deriving instance (Show x, Show a) => Show (Result x a)

instance Functor (Result x) where
  fmap = map

instance Applicative (Result x) where
  pure = Success

  Success function <*> Success value = Success (function value)
  Failure error <*> _ = Failure error
  _ <*> Failure error = Failure error

instance Monad (Result x) where
  Success value >>= function = function value
  Failure error >>= _ = Failure error

instance MonadFail (Result Text) where
  fail message = Failure (Text.pack message)

map :: (a -> b) -> Result x a -> Result x b
map function result = case result of
  Success value -> Success (function value)
  Failure error -> Failure error

map2 :: (a -> b -> c) -> Result x a -> Result x b -> Result x c
map2 function result1 result = do
  value1 <- result1
  value2 <- result
  Success (function value1 value2)

orFail :: MonadFail m => Result x a -> m a
orFail (Success value) = Prelude.return value
orFail (Failure error) = Prelude.fail (Text.unpack (Error.message error))

collect :: Traversable list => (a -> Result x b) -> list a -> Result x (list b)
collect = Prelude.mapM

foldl :: Foldable list => (b -> a -> Result x b) -> b -> list a -> Result x b
foldl = Data.Foldable.foldlM

foldr :: Foldable list => (a -> b -> Result x b) -> b -> list a -> Result x b
foldr = Data.Foldable.foldrM

sequence :: List (Result x a) -> Result x (List a)
sequence = Prelude.sequence
