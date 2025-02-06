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
  , combine
  , (>>=)
  , (>>)
  , toIO
  , fail
  )
where

import OpenSolid.Bootstrap
import OpenSolid.Composition
import OpenSolid.Error qualified as Error
import {-# SOURCE #-} OpenSolid.Text qualified as Text
import Prelude (Applicative, Functor, Monad, MonadFail)
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
  (>>=) = (>>=)

instance MonadFail (Result Text) where
  fail = fail

instance Composition (Result x ()) (Result x a) (Result x a) where
  Success () >> result = result
  Failure error >> _ = Failure error

instance Composition (IO ()) (Result x a) (IO a) where
  io >> result = io >> toIO result

instance Composition (Result x ()) (IO a) (IO a) where
  result >> io = toIO result >> io

(>>=) :: Result x a -> (a -> Result x b) -> Result x b
Success value >>= function = function value
Failure error >>= _ = Failure error

infixl 1 >>=

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
map2 function result1 result = OpenSolid.Result.do
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
collect = Prelude.mapM

foldl :: (b -> a -> Result x b) -> b -> List a -> Result x b
foldl _ accumulated [] = Success accumulated
foldl function accumulated (first : rest) =
  case function accumulated first of
    Success updated -> foldl function updated rest
    failure -> failure

foldr :: (a -> b -> Result x b) -> b -> List a -> Result x b
foldr _ accumulated [] = Success accumulated
foldr function accumulated (first : rest) =
  case foldr function accumulated rest of
    Success updated -> function first updated
    failure -> failure

combine :: List (Result x a) -> Result x (List a)
combine = Prelude.sequence

toIO :: Result x a -> IO a
toIO (Success value) = Prelude.return value
toIO (Failure error) = Prelude.fail (Text.unpack (Error.message error))

fail :: List Char -> Result Text a
fail = Text.pack >> Failure
