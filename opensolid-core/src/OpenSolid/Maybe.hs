module OpenSolid.Maybe
  ( Maybe (Just, Nothing)
  , map
  , map2
  , find
  , values
  , random
  , orElse
  , oneOf
  )
where

import OpenSolid.Bool qualified as Bool
import OpenSolid.List (List)
import OpenSolid.List qualified as List
import OpenSolid.Random qualified as Random
import Prelude (Foldable, Maybe (Just, Nothing))
import Prelude qualified

map :: (a -> b) -> Maybe a -> Maybe b
map = Prelude.fmap

map2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 function (Just first) second = map (function first) second
map2 _ Nothing _ = Nothing

find :: (a -> Maybe b) -> List a -> Maybe b
find _ [] = Nothing
find f (first : rest) = orElse (find f rest) (f first)

values :: Foldable list => list (Maybe a) -> List a
values maybes = Prelude.foldr List.prepend [] maybes

random :: Random.Generator a -> Random.Generator (Maybe a)
random randomValue = do
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
