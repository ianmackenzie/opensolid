module Coalesce (Coalesce ((??))) where

class Coalesce a b c | a b -> c where
  (??) :: a -> b -> c

infixr 2 ??
