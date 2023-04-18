module Result
  ( Result (..)
  , IsError (..)
  , map
  , withDefault
  , mapError
  , onError
  , orNothing
  )
where

import Basics
import DoNotation
import List qualified
import System.IO.Error qualified
import Text qualified
import Prelude qualified

class IsError error where
  errorMessage :: error -> Text

instance IsError Text where
  errorMessage text = text

instance IsError (List Char) where
  errorMessage = Text.fromChars

instance IsError IOError where
  errorMessage ioError = Text.fromChars (System.IO.Error.ioeGetErrorString ioError)

data Result x a where
  Ok :: a -> Result x a
  Error :: IsError x => x -> Result x a

deriving instance (Eq x, Eq a) => Eq (Result x a)

deriving instance (Show x, Show a) => Show (Result x a)

instance Prelude.Functor (Result x) where
  fmap f (Ok value) = Ok (f value)
  fmap _ (Error error) = Error error

instance Prelude.Applicative (Result x) where
  pure = Ok
  Ok function <*> Ok value = Ok (function value)
  Error error <*> _ = Error error
  Ok _ <*> Error error = Error error

instance Prelude.Monad (Result x) where
  Ok value >>= function = function value
  Error error >>= _ = Error error

instance Prelude.MonadFail (Result (List Char)) where
  fail = Error

instance x ~ x' => Bind (Result x) (Result x' b) where
  bind f (Ok value) = f value
  bind _ (Error error) = Error error

instance Bind [] (Result x (List b)) where
  bind f list =
    List.map f list
      |> List.collate
      |> Result.map List.concat

instance Fail (Result Text a) where
  fail = Error

withDefault :: a -> Result x a -> a
withDefault _ (Ok value) = value
withDefault fallback (Error _) = fallback

map :: (a -> value) -> Result x a -> Result x value
map function (Ok value) = Ok (function value)
map _ (Error err) = Error err

mapError :: IsError y => (x -> y) -> Result x a -> Result y a
mapError _ (Ok value) = Ok value
mapError function (Error err) = Error (function err)

onError :: (x -> Result y a) -> Result x a -> Result y a
onError _ (Ok value) = Ok value
onError function (Error error) = function error

orNothing :: Result x a -> Maybe a
orNothing (Ok value) = Just value
orNothing (Error _) = Nothing
