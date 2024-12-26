module OpenSolid.Bounds (Interface (..)) where

import OpenSolid.Prelude

class Interface b where
  aggregate2 :: b -> b -> b
  intersection :: b -> b -> Maybe b
