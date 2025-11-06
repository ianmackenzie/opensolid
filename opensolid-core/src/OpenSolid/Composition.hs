module OpenSolid.Composition (Composition (compose)) where

class Composition a b c | a b -> c where
  compose :: b -> a -> c

infixr 9 `compose`
