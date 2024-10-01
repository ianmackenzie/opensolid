module Maybe
  ( Maybe (Just, Nothing)
  , map
  , map2
  , withDefault
  , find
  , collect
  , values
  , any
  , all
  , (>>=)
  , andThen
  , (>>)
  , return
  , random
  , orElse
  , oneOf
  )
where

import Basics
import Bool qualified
import Composition
import Data.Maybe qualified
import Random qualified

map :: (a -> b) -> Maybe a -> Maybe b
map function (Just value) = Just (function value)
map _ Nothing = Nothing

map2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 function (Just first) second = map (function first) second
map2 _ Nothing _ = Nothing

return :: a -> Maybe a
return = Just

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Just value >>= function = function value
Nothing >>= _ = Nothing

andThen :: (a -> Maybe b) -> Maybe a -> Maybe b
andThen function maybe = maybe >>= function

infixl 1 >>=

withDefault :: a -> Maybe a -> a
withDefault _ (Just value) = value
withDefault value Nothing = value

find :: (a -> Maybe b) -> List a -> Maybe b
find _ [] = Nothing
find f (first : rest) = f first |> orElse (find f rest)

collect :: (a -> Maybe b) -> List a -> List b
collect = Data.Maybe.mapMaybe

values :: List (Maybe a) -> List a
values = Data.Maybe.catMaybes

any :: List (Maybe a) -> Maybe a
any (Just first : _) = Just first
any (Nothing : rest) = any rest
any [] = Nothing

all :: List (Maybe a) -> Maybe (List a)
all (Just first : rest) = map (first :) (all rest)
all (Nothing : _) = Nothing
all [] = Just []

random :: Random.Generator a -> Random.Generator (Maybe a)
random randomValue = Random.do
  generateJust <- Bool.random
  if generateJust
    then Random.map Just randomValue
    else Random.return Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse second first = case first of
  Just _ -> first
  Nothing -> second

oneOf :: List (Maybe a) -> Maybe a
oneOf maybes = case maybes of
  Just value : _ -> Just value
  Nothing : rest -> oneOf rest
  [] -> Nothing
