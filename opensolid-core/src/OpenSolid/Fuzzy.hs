module OpenSolid.Fuzzy
  ( Fuzzy (Resolved, Unresolved)
  , collect
  , oneOf
  , map
  , fromMaybe
  )
where

import OpenSolid.List (List)
import Prelude
  ( Applicative
  , Eq
  , Functor
  , Maybe (Just, Nothing)
  , Monad
  , Show
  , Traversable
  )
import Prelude qualified

data Fuzzy a = Resolved a | Unresolved deriving (Eq, Show)

instance Functor Fuzzy where
  fmap = map

instance Applicative Fuzzy where
  pure = Resolved

  Resolved function <*> Resolved value = Resolved (function value)
  Unresolved <*> _ = Unresolved
  Resolved _ <*> Unresolved = Unresolved

instance Monad Fuzzy where
  Resolved value >>= function = function value
  Unresolved >>= _ = Unresolved

fromMaybe :: Maybe a -> Fuzzy a
fromMaybe (Just value) = Resolved value
fromMaybe Nothing = Unresolved

map :: (a -> b) -> Fuzzy a -> Fuzzy b
map f (Resolved value) = Resolved (f value)
map _ Unresolved = Unresolved

collect :: Traversable list => (a -> Fuzzy b) -> list a -> Fuzzy (list b)
collect = Prelude.mapM

oneOf :: List (Fuzzy a) -> Fuzzy a
oneOf fuzzies = case fuzzies of
  Resolved value : _ -> Resolved value
  Unresolved : rest -> oneOf rest
  [] -> Unresolved
