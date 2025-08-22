module OpenSolid.Degenerate (value, bounds) where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Prelude

value :: Float -> a -> a -> a -> a
value t start middle end
  -- The constants here should be kept in sync with the LEFT/RIGHT constants in bytecode.cpp
  | t <= 0.000001 = start
  | t >= 0.999999 = end
  | otherwise = middle

bounds :: Bounds Unitless -> a -> a -> a -> a
bounds t start middle end
  -- The constants here should be kept in sync with the LEFT/RIGHT constants in bytecode.cpp
  | t.lower <= 0.000001 = start
  | t.upper >= 0.999999 = end
  | otherwise = middle
