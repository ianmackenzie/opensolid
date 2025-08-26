module OpenSolid.Desingularization
  ( t0
  , t1
  , value
  , bounds
  , continuity
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Prelude

t0 :: Float
-- Should be kept in sync with T0 in bytecode.cpp
t0 = 0.001

t1 :: Float
-- Should be kept in sync with CUTOFF_1 in bytecode.cpp
t1 = 0.999

value :: Float -> a -> a -> a -> a
value t start middle end
  | t <= t0 = start
  | t >= t1 = end
  | otherwise = middle

bounds :: Bounds Unitless -> a -> a -> a -> a
bounds t start middle end
  | t.upper <= t0 = start
  | t.lower >= t1 = end
  | otherwise = middle

{-| The order of continuity to use when joining a synthetic curve to a base curve
in order to 'desingularize' the base curve.
-}
continuity :: Int
continuity = 2
