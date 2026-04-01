module OpenSolid.Fuzzy
  ( Fuzzy (Resolved, Unresolved)
  , collect
  , oneOf
  , map
  , fromMaybe
  )
where

import OpenSolid.Prelude
import Prelude qualified

fromMaybe :: Maybe a -> Fuzzy a
fromMaybe (Just value) = Resolved value
fromMaybe Nothing = Unresolved

map :: (a -> b) -> Fuzzy a -> Fuzzy b
map = Prelude.fmap

collect :: Traversable list => (a -> Fuzzy b) -> list a -> Fuzzy (list b)
collect = Prelude.mapM

oneOf :: List (Fuzzy a) -> Fuzzy a
oneOf fuzzies = case fuzzies of
  Resolved value : _ -> Resolved value
  Unresolved : rest -> oneOf rest
  [] -> Unresolved
