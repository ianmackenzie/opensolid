module OpenSolid.Bounded (Bounded (bounds)) where

class Bounded a b where
  bounds :: a -> b
