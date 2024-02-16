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
  , (<*>)
  , (>>)
  , fmap
  , join
  , pure
  , return
  , fail
  )
where

import Basics hiding ((>>))
import Control.Monad (join)
import Error (Error)
import Error qualified
import Prelude (Applicative, Functor, Monad, MonadFail)
import Prelude qualified

data Result x a where
  Ok :: a -> Result x a
  Error :: Error x => x -> Result x a

instance (Error x, Error y, a ~ a') => Error.Map x y (Result x a) (Result y a') where
  map _ (Ok value) = Ok value
  map function (Error err) = Error (function err)

deriving instance (Eq x, Eq a) => Eq (Result x a)

deriving instance (Show x, Show a) => Show (Result x a)

instance Functor (Result x) where
  fmap = fmap

instance Applicative (Result x) where
  pure = pure
  (<*>) = (<*>)

instance Monad (Result x) where
  (>>=) = (>>=)

instance MonadFail (Result String) where
  fail = fail

pure :: a -> Result x a
pure = Ok

fmap :: (a -> b) -> Result x a -> Result x b
fmap = map

(<*>) :: Result x (a -> b) -> Result x a -> Result x b
Ok function <*> Ok value = Ok (function value)
Error error <*> _ = Error error
Ok _ <*> Error error = Error error

(>>=) :: Result x a -> (a -> Result x b) -> Result x b
Ok value >>= function = function value
Error error >>= _ = Error error

(>>) :: Result x () -> Result x a -> Result x a
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
map2 function result1 result2 = Prelude.do
  value1 <- result1
  value2 <- result2
  return (function value1 value2)

check :: Error x => Bool -> x -> Result x ()
check condition error = if condition then Ok () else Error error

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
