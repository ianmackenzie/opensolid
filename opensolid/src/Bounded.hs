module Bounded (Interface (..)) where

import Bounds qualified

class (Bounds.Interface b) => Interface a b | a -> b where
  bounds :: a -> b
