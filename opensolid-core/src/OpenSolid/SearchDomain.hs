module OpenSolid.SearchDomain
  ( Bounds
  , touching
  , contains
  , overlapping
  )
where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.Set qualified as Set
import OpenSolid.UvBounds (UvBounds)

class (Set.Bounds bounds, Intersects bounds bounds (Tolerance Unitless)) => Bounds bounds where
  contains :: bounds -> bounds -> Bool
  overlap :: bounds -> bounds -> Number

instance Bounds (Interval Unitless) where
  contains = Interval.contains
  overlap = Interval.overlap

instance Bounds UvBounds where
  contains = Bounds2D.contains
  overlap = Bounds2D.overlap

instance (Bounds bounds1, Bounds bounds2) => Bounds (bounds1, bounds2) where
  contains (b1, b2) (a1, a2) = contains b1 a1 && contains b2 a2
  overlap (b1, b2) (a1, a2) = min (overlap b1 a1) (overlap b2 a2)

touching :: Bounds b => b -> b -> Bool
touching bounds1 bounds2 = overlap bounds1 bounds2 >= 0.0

overlapping :: Bounds b => b -> b -> Bool
overlapping bounds1 bounds2 = overlap bounds1 bounds2 > 0.0
