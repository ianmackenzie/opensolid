module OpenSolid.Bounds
  ( Bounds
  , Exists
  , contains
  , aggregate2
  , cyclicCoordinate
  , diameter
  )
where

import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Interval (Interval)
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D (Bounds2D), Bounds3D (Bounds3D))
import OpenSolid.VectorBounds qualified as VectorBounds

type family Bounds dimension units space = bounds | bounds -> dimension units space where
  Bounds 2 units space = Bounds2D units space
  Bounds 3 Meters space = Bounds3D space

class
  ( Point.Exists dimension units space
  , VectorBounds.Exists dimension units space
  , Show (Bounds dimension units space)
  , Intersects (Point dimension units space) (Bounds dimension units space) (Tolerance units)
  , Intersects (Bounds dimension units space) (Point dimension units space) (Tolerance units)
  , Intersects (Bounds dimension units space) (Bounds dimension units space) (Tolerance units)
  ) =>
  Exists dimension units space
  where
  contains :: Bounds dimension units space -> Bounds dimension units space -> Bool
  aggregate2 ::
    Bounds dimension units space ->
    Bounds dimension units space ->
    Bounds dimension units space
  cyclicCoordinate :: Int -> Bounds dimension units space -> Interval units
  diameter :: Bounds dimension units space -> Quantity units

instance Exists 2 units space where
  contains = Bounds2D.contains
  aggregate2 = Bounds2D.aggregate2
  cyclicCoordinate i (Bounds2D x y) = case i % 2 of 0 -> x; _ -> y
  diameter = Bounds2D.diameter

instance Exists 3 Meters space where
  contains = Bounds3D.contains
  aggregate2 = Bounds3D.aggregate2
  cyclicCoordinate i (Bounds3D x y z) = case i % 3 of 0 -> x; 1 -> y; _ -> z
  diameter = Bounds3D.diameter
