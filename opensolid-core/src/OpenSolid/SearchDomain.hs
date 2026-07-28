module OpenSolid.SearchDomain
  ( Bounds
  , contains
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

instance Bounds (Interval Unitless) where
  contains = Interval.contains

instance Bounds UvBounds where
  contains = Bounds2D.contains

instance (Bounds bounds1, Bounds bounds2) => Bounds (bounds1, bounds2) where
  contains (b1, b2) (a1, a2) = contains b1 a1 && contains b2 a2
