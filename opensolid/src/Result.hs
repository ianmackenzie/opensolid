module Result
  ( Result (Success, Failure)
  , map
  , map2
  , andThen
  , addContext
  , onError
  , try
  , collect
  , combine
  , (>>=)
  , (>>)
  )
where

import Basics
import Coalesce (Coalesce ((??)))
import Composition
import Error qualified
import System.IO.Error
import {-# SOURCE #-} Text qualified
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
  fail = Text.pack >> Failure

instance Composition (Result x ()) (Result x a) (Result x a) where
  Success () >> result = result
  Failure error >> _ = Failure error

instance a1 ~ a2 => Coalesce (Maybe a1) (Result x a2) (Result x a1) where
  Just value ?? _ = Success value
  Nothing ?? fallback = fallback

instance a1 ~ a2 => Coalesce (IO a1) (Result x a2) (IO a1) where
  io ?? fallback =
    System.IO.Error.catchIOError io $
      \_ -> case fallback of
        Success value -> Prelude.return value
        Failure error -> Prelude.fail (Text.unpack (Error.message error))

(>>=) :: Result x a -> (a -> Result x b) -> Result x b
Success value >>= function = function value
Failure error >>= _ = Failure error

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
map2 function result1 result = Result.do
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

collect :: (a -> Result x b) -> List a -> Result x (List b)
collect = Prelude.mapM

combine :: List (Result x a) -> Result x (List a)
combine = Prelude.sequence
