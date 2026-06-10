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
import OpenSolid.Quantity qualified as Quantity

{-# INLINE intervalMidpoint #-}
intervalMidpoint :: Interval units -> Number
intervalMidpoint interval = Quantity.erase (Interval.midpoint interval)

class Bounds bounds where
  dimension :: Proxy bounds -> Int
  sortValue :: Int -> bounds -> Number
  aggregateOf :: (a -> bounds) -> NonEmpty a -> bounds

instance Bounds (Interval units) where
  dimension _ = 1
  sortValue index interval = case index of
    0 -> intervalMidpoint interval
    _ -> throw IndexOutOfBounds{index, size = 1}
  aggregateOf = Interval.aggregateOf

instance Bounds (Bounds2D units) where
  dimension _ = 2
  sortValue index (Bounds2D x y) = case index of
    0 -> intervalMidpoint x
    1 -> intervalMidpoint y
    _ -> throw IndexOutOfBounds{index, size = 2}
  aggregateOf = Bounds2D.aggregateOf

instance Bounds (Bounds3D space) where
  dimension _ = 3
  sortValue index (Bounds3D x y z) = case index of
    0 -> intervalMidpoint x
    1 -> intervalMidpoint y
    2 -> intervalMidpoint z
    _ -> throw IndexOutOfBounds{index, size = 3}
  aggregateOf = Bounds3D.aggregateOf

instance (Bounds bounds1, Bounds bounds2) => Bounds (bounds1, bounds2) where
  dimension _ = dimension @bounds1 Proxy + dimension @bounds2 Proxy
  sortValue = do
    let dimension1 = dimension @bounds1 Proxy
    let dimension2 = dimension @bounds2 Proxy
    let outOfBounds index = throw IndexOutOfBounds{index, size = dimension1 + dimension2}
    \index (bounds1, bounds2) ->
      if
        | index < 0 -> outOfBounds index
        | index < dimension1 -> sortValue index bounds1
        | let index2 = index - dimension1, index2 < dimension2 -> sortValue index2 bounds2
        | otherwise -> outOfBounds index
  aggregateOf getBounds nonEmpty =
    ( aggregateOf (Pair.first . getBounds) nonEmpty
    , aggregateOf (Pair.second . getBounds) nonEmpty
    )
