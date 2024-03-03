module Fuzzy
  ( Fuzzy (Resolved, Unresolved)
  , and
  , or
  , collect
  , (>>=)
  , (<*>)
  , fmap
  , join
  , pure
  , return
  )
where

import Basics hiding (pure)
import Control.Monad (join)
import Prelude (Applicative, Functor, Monad)
import Prelude qualified

data Fuzzy a = Resolved a | Unresolved deriving (Eq, Show)

instance Functor Fuzzy where
  fmap = fmap

instance Applicative Fuzzy where
  pure = pure
  (<*>) = (<*>)

instance Monad Fuzzy where
  (>>=) = (>>=)

(>>=) :: Fuzzy a -> (a -> Fuzzy b) -> Fuzzy b
Resolved value >>= function = function value
Unresolved >>= _ = Unresolved

(<*>) :: Fuzzy (a -> b) -> Fuzzy a -> Fuzzy b
Resolved function <*> Resolved value = Resolved (function value)
Unresolved <*> _ = Unresolved
Resolved _ <*> Unresolved = Unresolved

pure :: a -> Fuzzy a
pure = Resolved

fmap :: (a -> b) -> Fuzzy a -> Fuzzy b
fmap f (Resolved value) = Resolved (f value)
fmap _ Unresolved = Unresolved

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
