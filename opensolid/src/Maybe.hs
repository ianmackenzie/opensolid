module Maybe
  ( Maybe (Just, Nothing)
  , map
  , withDefault
  , orError
  , collect
  , values
  )
where

import Basics
import Data.Maybe qualified
import Result (ErrorMessage, Result (Error, Ok))
import Prelude qualified

map :: (a -> b) -> Maybe a -> Maybe b
map = Prelude.fmap

withDefault :: a -> Maybe a -> a
withDefault _ (Just value) = value
withDefault value Nothing = value

orError :: ErrorMessage x => x -> Maybe a -> Result x a
orError _ (Just value) = Ok value
orError error Nothing = Error error

collect :: (a -> Maybe b) -> List a -> List b
collect = Data.Maybe.mapMaybe

values :: List (Maybe a) -> List a
values = Data.Maybe.catMaybes
