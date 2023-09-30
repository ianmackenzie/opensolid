module Bounded (IsBounded (..)) where

import Bounds

class (IsBounds b) => IsBounded a b | a -> b where
  boundsImpl :: a -> b
