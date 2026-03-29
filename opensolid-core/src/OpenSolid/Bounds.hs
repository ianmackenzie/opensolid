module OpenSolid.Bounds
  ( Bounds
  , Exists
  , constant
  , contains
  , hull2
  , aggregate2
  , intersection
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
import OpenSolid.VectorBounds (VectorBounds)
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
  , Addition (Point dimension units space) (VectorBounds dimension units space) (Bounds dimension units space)
  , Subtraction (Point dimension units space) (VectorBounds dimension units space) (Bounds dimension units space)
  ) =>
  Exists dimension units space
  where
  constant :: Point dimension units space -> Bounds dimension units space
  contains :: Bounds dimension units space -> Bounds dimension units space -> Bool
  hull2 :: Point dimension units space -> Point dimension units space -> Bounds dimension units space
  aggregate2 ::
    Bounds dimension units space ->
    Bounds dimension units space ->
    Bounds dimension units space
  intersection ::
    Bounds dimension units space ->
    Bounds dimension units space ->
    Maybe (Bounds dimension units space)
  cyclicCoordinate :: Int -> Bounds dimension units space -> Interval units
  diameter :: Bounds dimension units space -> Quantity units

instance Exists 2 units space where
  constant = Bounds2D.constant
  contains = Bounds2D.contains
  hull2 = Bounds2D.hull2
  aggregate2 = Bounds2D.aggregate2
  intersection = Bounds2D.intersection
  cyclicCoordinate i (Bounds2D x y) = case i % 2 of 0 -> x; _ -> y
  diameter = Bounds2D.diameter

instance Exists 3 Meters space where
  constant = Bounds3D.constant
  contains = Bounds3D.contains
  hull2 = Bounds3D.hull2
  aggregate2 = Bounds3D.aggregate2
  intersection = Bounds3D.intersection
  cyclicCoordinate i (Bounds3D x y z) = case i % 3 of 0 -> x; 1 -> y; _ -> z
  diameter = Bounds3D.diameter
