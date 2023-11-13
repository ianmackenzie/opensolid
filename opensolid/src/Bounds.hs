module Bounds (Interface (..)) where

import OpenSolid

class Interface b where
  aggregate2Impl :: b -> b -> b
  intersectionImpl :: b -> b -> Maybe b
