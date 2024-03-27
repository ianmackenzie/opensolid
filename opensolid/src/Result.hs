module Result
  ( Result (Ok, Error)
  , map
  , map2
  , withDefault
  , check
  , onError
  , handleError
  , orNothing
  , collect
  , combine
  , (>>=)
  , (>>)
  , return
  , fail
  )
where

import Basics
import Coalesce (Coalesce ((??)))
import Composition
import Error (Error)
import Error qualified
import {-# SOURCE #-} IO qualified
import Prelude (Applicative, Functor, Monad, MonadFail)
import Prelude qualified

data Result x a where
  Ok :: a -> Result x a
  Error :: Error x => x -> Result x a

instance (Error x, Error y) => Error.Map x y (Result x) (Result y) where
  map _ (Ok value) = Ok value
  map function (Error error) = Error (function error)

deriving instance (Eq x, Eq a) => Eq (Result x a)

deriving instance (Show x, Show a) => Show (Result x a)

instance Functor (Result x) where
  fmap = map

instance Applicative (Result x) where
  pure = return

  Ok function <*> Ok value = Ok (function value)
  Error error <*> _ = Error error
  Ok _ <*> Error error = Error error

instance Monad (Result x) where
  (>>=) = (>>=)

instance MonadFail (Result String) where
  fail = fail

instance Composition (Result x ()) (IO a) (IO a) where
  Ok () >> io = io
  Error error >> _ = IO.fail (Error.message error)

instance a ~ a' => Coalesce (Result x a) (Result y a') (Result y a) where
  Ok value ?? _ = Ok value
  Error _ ?? fallback = fallback

return :: a -> Result x a
return = Ok

(>>=) :: Result x a -> (a -> Result x b) -> Result x b
Ok value >>= function = function value
Error error >>= _ = Error error

instance x ~ x' => Composition (Result x ()) (Result x' a) (Result x a) where
  Ok _ >> result = result
  Error error >> _ = Error error

fail :: Error x => x -> Result x a
fail = Error

withDefault :: a -> Result x a -> a
withDefault _ (Ok value) = value
withDefault fallback (Error _) = fallback

map :: (a -> value) -> Result x a -> Result x value
map f (Ok value) = Ok (f value)
map _ (Error error) = Error error

map2 :: (a -> b -> value) -> Result x a -> Result x b -> Result x value
map2 function result1 result2 = Result.do
  value1 <- result1
  value2 <- result2
  return (function value1 value2)

data CheckFailed = CheckFailed deriving (Eq, Show, Error)

check :: Bool -> Result CheckFailed ()
check condition = if condition then Ok () else Error CheckFailed

onError :: (x -> Result y a) -> Result x a -> Result y a
onError _ (Ok value) = Ok value
onError function (Error error) = function error

handleError :: (x -> a) -> Result x a -> a
handleError _ (Ok value) = value
handleError function (Error error) = function error

orNothing :: Result x a -> Maybe a
orNothing (Ok value) = Just value
orNothing (Error _) = Nothing

collect :: (a -> Result x b) -> List a -> Result x (List b)
collect = Prelude.mapM

combine :: List (Result x a) -> Result x (List a)
combine = Prelude.sequence
