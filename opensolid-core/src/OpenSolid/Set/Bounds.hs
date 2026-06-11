module OpenSolid.Set.Bounds (Bounds (..)) where

import Data.Proxy (Proxy (Proxy))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.IndexOutOfBounds (IndexOutOfBounds (..))
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D (Bounds2D), Bounds3D (Bounds3D))

class Bounds bounds where
  dimension :: Proxy bounds -> Int
  component :: Int -> bounds -> Interval Unitless
  aggregateOf :: (a -> bounds) -> NonEmpty a -> bounds

instance Bounds (Interval units) where
  dimension _ = 1
  component index interval = case index of
    0 -> Interval.erase interval
    _ -> throw IndexOutOfBounds{index, size = 1}
  aggregateOf = Interval.aggregateOf

instance Bounds (Bounds2D units) where
  dimension _ = 2
  component index (Bounds2D x y) = case index of
    0 -> Interval.erase x
    1 -> Interval.erase y
    _ -> throw IndexOutOfBounds{index, size = 2}
  aggregateOf = Bounds2D.aggregateOf

instance Bounds (Bounds3D space) where
  dimension _ = 3
  component index (Bounds3D x y z) = case index of
    0 -> Interval.erase x
    1 -> Interval.erase y
    2 -> Interval.erase z
    _ -> throw IndexOutOfBounds{index, size = 3}
  aggregateOf = Bounds3D.aggregateOf

instance (Bounds bounds1, Bounds bounds2) => Bounds (bounds1, bounds2) where
  dimension _ = dimension @bounds1 Proxy + dimension @bounds2 Proxy
  component = do
    let dimension1 = dimension @bounds1 Proxy
    let dimension2 = dimension @bounds2 Proxy
    let outOfBounds index = throw IndexOutOfBounds{index, size = dimension1 + dimension2}
    \index (bounds1, bounds2) ->
      if
        | index < 0 -> outOfBounds index
        | index < dimension1 -> component index bounds1
        | let index2 = index - dimension1, index2 < dimension2 -> component index2 bounds2
        | otherwise -> outOfBounds index
  aggregateOf getBounds nonEmpty =
    ( aggregateOf (Pair.first . getBounds) nonEmpty
    , aggregateOf (Pair.second . getBounds) nonEmpty
    )
