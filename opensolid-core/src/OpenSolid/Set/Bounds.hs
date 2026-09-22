module OpenSolid.Set.Bounds (Bounds (..)) where

import Data.Proxy (Proxy (Proxy))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D qualified as Bounds3D
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

indexOutOfBounds :: Text
indexOutOfBounds = "Index out of bounds"

instance Bounds (Interval units) where
  dimension _ = 1
  sortValue index interval = case index of
    0 -> intervalMidpoint interval
    _ -> error indexOutOfBounds
  aggregateOf = Interval.aggregateOf

instance Bounds (Bounds2D units) where
  dimension _ = 2
  sortValue index (Bounds2D x y) = case index of
    0 -> intervalMidpoint x
    1 -> intervalMidpoint y
    _ -> error indexOutOfBounds
  aggregateOf = Bounds2D.aggregateOf

instance Bounds (Bounds3D space) where
  dimension _ = 3
  sortValue index (Bounds3D x y z) = case index of
    0 -> intervalMidpoint x
    1 -> intervalMidpoint y
    2 -> intervalMidpoint z
    _ -> error indexOutOfBounds
  aggregateOf = Bounds3D.aggregateOf

instance (Bounds bounds1, Bounds bounds2) => Bounds (bounds1, bounds2) where
  dimension _ = dimension @bounds1 Proxy + dimension @bounds2 Proxy
  sortValue = do
    let dimension1 = dimension @bounds1 Proxy
    let dimension2 = dimension @bounds2 Proxy
    \index (bounds1, bounds2) ->
      if
        | index < 0 -> error indexOutOfBounds
        | index < dimension1 -> sortValue index bounds1
        | let index2 = index - dimension1, index2 < dimension2 -> sortValue index2 bounds2
        | otherwise -> error indexOutOfBounds
  aggregateOf getBounds nonEmpty =
    ( aggregateOf (Pair.first . getBounds) nonEmpty
    , aggregateOf (Pair.second . getBounds) nonEmpty
    )
