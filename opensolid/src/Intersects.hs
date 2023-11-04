module Intersects (Intersects ((^))) where

import Basics
import Tolerance (Tolerance)

class (Intersects b a units) => Intersects a b units | a b -> units where
  (^) :: (Tolerance units) => a -> b -> Bool
