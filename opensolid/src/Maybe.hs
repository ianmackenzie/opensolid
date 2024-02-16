module Maybe
  ( Maybe (Just, Nothing)
  , map
  , withDefault
  , orError
  , collect
  , values
  , (>>=)
  , (<*>)
  , fmap
  , join
  , pure
  , return
  )
where

import Basics
import Control.Monad (join)
import Data.Maybe qualified
import Error (Error)
import Result (Result (Error, Ok))

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap = map

map :: (a -> b) -> Maybe a -> Maybe b
map function (Just value) = Just (function value)
map _ Nothing = Nothing

pure :: a -> Maybe a
pure = Just

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Just value >>= function = function value
Nothing >>= _ = Nothing

(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
Just function <*> Just value = Just (function value)
Nothing <*> _ = Nothing
_ <*> Nothing = Nothing

withDefault :: a -> Maybe a -> a
withDefault _ (Just value) = value
withDefault value Nothing = value

orError :: Error x => x -> Maybe a -> Result x a
orError _ (Just value) = Ok value
orError error Nothing = Error error

collect :: (a -> Maybe b) -> List a -> List b
collect = Data.Maybe.mapMaybe

values :: List (Maybe a) -> List a
values = Data.Maybe.catMaybes
