module Intersect (Intersect ((^))) where

import Tolerance (Tolerance)

class
  (Intersect b a units c) =>
  Intersect a b units c
    | a b -> units
    , a b -> c
  where
  (^) :: (Tolerance units) => a -> b -> c
