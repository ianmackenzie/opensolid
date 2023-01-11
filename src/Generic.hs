module Generic (Zero (..)) where

class Zero p where
  zero :: p a
