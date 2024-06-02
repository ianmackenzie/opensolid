module Fuzzy
  ( Fuzzy (Resolved, Unresolved)
  , collect
  , (>>=)
  , (>>)
  , map
  , return
  )
where

import Basics
import Composition
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

(>>=) :: Fuzzy a -> (a -> Fuzzy b) -> Fuzzy b
Resolved value >>= function = function value
Unresolved >>= _ = Unresolved

return :: a -> Fuzzy a
return = Resolved

map :: (a -> b) -> Fuzzy a -> Fuzzy b
map f (Resolved value) = Resolved (f value)
map _ Unresolved = Unresolved

collect :: (a -> Fuzzy b) -> List a -> Fuzzy (List b)
collect = Prelude.mapM
