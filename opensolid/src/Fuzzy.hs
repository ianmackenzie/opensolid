module Fuzzy
  ( Fuzzy (Resolved, Unresolved)
  , and
  , or
  , collect
  )
where

import Basics
import Prelude qualified

data Fuzzy a = Resolved a | Unresolved deriving (Eq, Show)

instance Prelude.Functor Fuzzy where
  fmap f (Resolved value) = Resolved (f value)
  fmap _ Unresolved = Unresolved

instance Prelude.Applicative Fuzzy where
  pure = Resolved
  Resolved function <*> Resolved value = Resolved (function value)
  Unresolved <*> _ = Unresolved
  Resolved _ <*> Unresolved = Unresolved

instance Prelude.Monad Fuzzy where
  Resolved value >>= function = function value
  Unresolved >>= _ = Unresolved

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
collect _ [] = Resolved []
collect function (first : rest) = do
  resolvedFirst <- function first
  resolvedRest <- collect function rest
  Resolved (resolvedFirst : resolvedRest)
