module Result
  ( Result (..)
  , IsError (..)
  , map
  , withDefault
  , mapError
  )
where

import Basics
import DoNotation
import List qualified
import System.IO.Error qualified
import Text qualified

class IsError error where
  errorMessage :: error -> Text

instance IsError Text where
  errorMessage text = text

instance IsError IOError where
  errorMessage ioError = Text.fromChars (System.IO.Error.ioeGetErrorString ioError)

data Result x a where
  Ok :: a -> Result x a
  Error :: IsError x => x -> Result x a

deriving instance (Eq x, Eq a) => Eq (Result x a)

deriving instance (Show x, Show a) => Show (Result x a)

instance x ~ x' => Bind (Result x) (Result x' b) where
  Ok value >>= function = function value
  Error error >>= _ = Error error

instance Bind [] (Result x (List b)) where
  list >>= function =
    List.map function list
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
