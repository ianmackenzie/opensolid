module OpenSolid.Bounded (Bounded (bounds)) where

class Bounded a b where
  bounds :: a -> b

instance (Bounded a1 b1, Bounded a2 b2) => Bounded (a1, a2) (b1, b2) where
  bounds (a1, a2) = (bounds a1, bounds a2)
