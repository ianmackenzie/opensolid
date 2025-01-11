module OpenSolid.Bounds (Bounds (..)) where

import {-# SOURCE #-} OpenSolid.Bounded (Bounded)
import OpenSolid.Prelude

class Bounded b b => Bounds b where
  aggregate2 :: b -> b -> b
  intersection :: b -> b -> Maybe b
