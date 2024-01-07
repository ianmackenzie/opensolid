module Bounds (Interface (..)) where

import OpenSolid

class Interface b where
  aggregate2 :: b -> b -> b
  intersection :: b -> b -> Maybe b
