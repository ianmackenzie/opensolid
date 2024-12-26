module OpenSolid.Fuzzy
  ( Fuzzy (Resolved, Unresolved)
  , collect
  , oneOf
  , (>>=)
  , (>>)
  , map
  , return
  , fromMaybe
  )
where

import OpenSolid.Bootstrap
import OpenSolid.Composition
import Prelude (Applicative, Functor, Monad)
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
  (>>=) = (>>=)

infixl 1 >>=

(>>=) :: Fuzzy a -> (a -> Fuzzy b) -> Fuzzy b
Resolved value >>= function = function value
Unresolved >>= _ = Unresolved

return :: a -> Fuzzy a
return = Resolved

fromMaybe :: Maybe a -> Fuzzy a
fromMaybe (Just value) = Resolved value
fromMaybe Nothing = Unresolved

map :: (a -> b) -> Fuzzy a -> Fuzzy b
map f (Resolved value) = Resolved (f value)
map _ Unresolved = Unresolved

collect :: (a -> Fuzzy b) -> List a -> Fuzzy (List b)
collect = Prelude.mapM

oneOf :: List (Fuzzy a) -> Fuzzy a
oneOf fuzzies = case fuzzies of
  Resolved value : _ -> Resolved value
  Unresolved : rest -> oneOf rest
  [] -> Unresolved
