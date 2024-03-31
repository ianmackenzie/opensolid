module Coalesce (Coalesce ((??))) where

import Basics

class Coalesce a b c | a b -> c where
  (??) :: a -> b -> c

infixr 2 ??

instance a ~ a' => Coalesce (Maybe a) (Maybe a') (Maybe a) where
  Just value ?? _ = Just value
  Nothing ?? fallback = fallback
