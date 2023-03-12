module Result
  ( Result (..)
  , IsError (..)
  , (??)
  , map
  , andThen
  , withDefault
  , mapError
  , toMaybe
  )
where

import Basics
import DoNotation
import List qualified

class IsError error where
  errorMessage :: error -> Text

instance IsError Text where
  errorMessage text = text

data Result x a where
  Ok :: a -> Result x a
  Error :: IsError x => x -> Result x a

instance x ~ x' => Bind (Result x) (Result x' b) where
  Ok value >>= function = function value
  Error error >>= _ = Error error

instance Bind [] (Result x (List b)) where
  [] >>= _ = Ok []
  list >>= function =
    List.map function list
      |> List.collate
      |> Result.map List.concat

instance Fail (Result Text a) where
  fail = Error

(??) :: Result x a -> Result y a -> Result y a
Ok value ?? _ = Ok value
Error _ ?? fallback = fallback

infixl 0 ??

deriving instance (Eq x, Eq a) => Eq (Result x a)

deriving instance (Show x, Show a) => Show (Result x a)

withDefault :: a -> Result x a -> a
withDefault _ (Ok value) = value
withDefault fallback (Error _) = fallback

map :: (a -> value) -> Result x a -> Result x value
map function (Ok value) = Ok (function value)
map _ (Error err) = Error err

andThen :: (a -> Result x b) -> Result x a -> Result x b
andThen function (Ok value) = function value
andThen _ (Error err) = Error err

mapError :: IsError y => (x -> y) -> Result x a -> Result y a
mapError _ (Ok value) = Ok value
mapError function (Error err) = Error (function err)

toMaybe :: Result x a -> Maybe a
toMaybe (Ok value) = Just value
toMaybe (Error _) = Nothing
