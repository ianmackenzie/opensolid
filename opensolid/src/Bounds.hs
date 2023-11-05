module Bounds (IsBounds (..)) where

import OpenSolid

class IsBounds b where
  aggregate2Impl :: b -> b -> b
  intersectionImpl :: b -> b -> Maybe b
