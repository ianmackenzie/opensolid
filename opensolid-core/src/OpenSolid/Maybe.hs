module OpenSolid.Maybe
  ( Maybe (Just, Nothing)
  , map
  , map2
  , oneOf
  )
where

import OpenSolid.List (List)
import Prelude (Maybe (Just, Nothing))
import Prelude qualified

map :: (a -> b) -> Maybe a -> Maybe b
map = Prelude.fmap

map2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 function (Just first) second = map (function first) second
map2 _ Nothing _ = Nothing

oneOf :: List (Maybe a) -> Maybe a
oneOf maybes = case maybes of
  Just value : _ -> Just value
  Nothing : rest -> oneOf rest
  [] -> Nothing
