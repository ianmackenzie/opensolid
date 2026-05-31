module OpenSolid.Set.Bounds (Bounds (..)) where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Int qualified as Int
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D (Bounds2D), Bounds3D (Bounds3D))

class Bounds bounds where
  aggregate :: bounds -> bounds -> bounds
  cycle :: Int -> bounds -> Interval Unitless

instance Bounds (Interval units) where
  aggregate = Interval.aggregate2
  cycle _ interval = Interval.erase interval

instance Bounds (Bounds2D units) where
  aggregate = Bounds2D.aggregate2
  cycle i (Bounds2D x y) = Interval.erase (if Int.isEven i then x else y)

instance Bounds (Bounds3D space) where
  aggregate = Bounds3D.aggregate2
  cycle i (Bounds3D x y z) = Interval.erase (case i % 3 of 0 -> x; 1 -> y; _ -> z)

instance (Bounds bounds1, Bounds bounds2) => Bounds (bounds1, bounds2) where
  aggregate (a1, b1) (a2, b2) = (aggregate a1 a2, aggregate b1 b2)
  cycle i (a, b) =
    if Int.isEven i
      then cycle (i // 2) a
      else cycle (i // 2) b
