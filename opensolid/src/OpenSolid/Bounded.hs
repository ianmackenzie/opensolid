module OpenSolid.Bounded (Interface (..)) where

import OpenSolid.Bounds qualified as Bounds

class Bounds.Interface b => Interface a b | a -> b where
  bounds :: a -> b
