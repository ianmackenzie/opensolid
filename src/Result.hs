module Result
  ( Result (..)
  , ErrorMessage (..)
  , map
  , withDefault
  , mapError
  , onError
  , handleError
  , orNothing
  , collect
  , combine
  )
where

import Basics
import Data.Text qualified
import DoNotation
import System.IO.Error qualified
import Prelude qualified

class Show error => ErrorMessage error where
  errorMessage :: error -> Text
  errorMessage error = Data.Text.pack (Prelude.show error)

instance ErrorMessage Text where
  errorMessage text = text

instance ErrorMessage (List Char) where
  errorMessage = Data.Text.pack

instance ErrorMessage IOError where
  errorMessage ioError = Data.Text.pack (System.IO.Error.ioeGetErrorString ioError)

data Result x a where
  Ok :: a -> Result x a
  Error :: ErrorMessage x => x -> Result x a

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

instance (x ~ x', a ~ a') => Bind (Result x a) a' (Result x' b) where
  bind f (Ok value) = f value
  bind _ (Error error) = Error error

instance Fail (Result Text a) where
  fail = Error

withDefault :: a -> Result x a -> a
withDefault _ (Ok value) = value
withDefault fallback (Error _) = fallback

map :: (a -> value) -> Result x a -> Result x value
map function (Ok value) = Ok (function value)
map _ (Error err) = Error err

mapError :: ErrorMessage y => (x -> y) -> Result x a -> Result y a
mapError _ (Ok value) = Ok value
mapError function (Error err) = Error (function err)

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
collect _ [] = Ok []
collect f (a : as) = do b <- f a; bs <- collect f as; Ok (b : bs)

combine :: List (Result x a) -> Result x (List a)
combine [] = Ok []
combine (firstResult : followingResults) = do
  firstValue <- firstResult
  followingValues <- combine followingResults
  Ok (firstValue : followingValues)
