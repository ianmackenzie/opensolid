module Bounded (Bounded (..)) where

import Bounds

class Bounds b => Bounded a b | a -> b where
    bounds :: a -> b
