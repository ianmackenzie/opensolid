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
  , toIO
  )
where

import Composition
import Error qualified
import OpenSolid.Bootstrap
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
  fail = Text.pack >> Failure

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

toIO :: Result x a -> IO a
toIO (Success value) = Prelude.return value
toIO (Failure error) = Prelude.fail (Text.unpack (Error.message error))
