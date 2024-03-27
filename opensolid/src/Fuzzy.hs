module Fuzzy
  ( Fuzzy (Resolved, Unresolved)
  , and
  , or
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

and :: Fuzzy Bool -> Fuzzy Bool -> Fuzzy Bool
and (Resolved False) _ = Resolved False
and _ (Resolved False) = Resolved False
and (Resolved True) other = other
and other (Resolved True) = other
and Unresolved Unresolved = Unresolved

or :: Fuzzy Bool -> Fuzzy Bool -> Fuzzy Bool
or (Resolved True) _ = Resolved True
or _ (Resolved True) = Resolved True
or (Resolved False) other = other
or other (Resolved False) = other
or Unresolved Unresolved = Unresolved

collect :: (a -> Fuzzy b) -> List a -> Fuzzy (List b)
collect = Prelude.mapM
