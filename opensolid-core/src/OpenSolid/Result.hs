module OpenSolid.Result
  ( Result (Success, Failure)
  , map
  , map2
  , andThen
  , addContext
  , onError
  , try
  , collect
  , foldl
  , foldr
  , sequence
  , (>>=)
  , fail
  )
where

import OpenSolid.Bootstrap hiding (foldl, foldr, sequence)
import OpenSolid.Error qualified as Error
import {-# SOURCE #-} OpenSolid.Text qualified as Text
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

andThen :: (a -> Result x b) -> Result x a -> Result x b
andThen function result = result >>= function

map :: (a -> b) -> Result x a -> Result x b
map function result = case result of
  Success value -> Success (function value)
  Failure error -> Failure error

addContext :: Text -> Result x a -> Result Text a
addContext context result = case result of
  Success value -> Success value
  Failure error -> Failure (Error.addContext context (Error.message error))

map2 :: (a -> b -> c) -> Result x a -> Result x b -> Result x c
map2 function result1 result = do
  value1 <- result1
  value2 <- result
  Success (function value1 value2)

onError :: (x -> Result y a) -> Result x a -> Result y a
onError function result = case result of
  Success value -> Success value
  Failure error -> function error

try :: Result x a -> Result Text a
try result = case result of
  Success value -> Success value
  Failure error -> Failure (Error.message error)

collect :: Traversable list => (a -> Result x b) -> list a -> Result x (list b)
collect = mapM

applyForward :: (b -> a -> Result x b) -> Result x b -> a -> Result x b
applyForward f (Success acc) item = f acc item
applyForward _ failure _ = failure

foldl :: Foldable list => (b -> a -> Result x b) -> b -> list a -> Result x b
foldl function init list = Prelude.foldl' (applyForward function) (Success init) list

applyBackward :: (a -> b -> Result x b) -> a -> Result x b -> Result x b
applyBackward f item (Success acc) = f item acc
applyBackward _ _ failure = failure

foldr :: Foldable list => (a -> b -> Result x b) -> b -> list a -> Result x b
foldr function init list = Prelude.foldr (applyBackward function) (Success init) list

sequence :: List (Result x a) -> Result x (List a)
sequence = Prelude.sequence
