module Bounds (IsBounds (..)) where

import OpenSolid

class IsBounds b where
  aggregate2Impl :: b -> b -> b
  intersectsImpl :: b -> b -> Bool
  intersectionImpl :: b -> b -> Maybe b
