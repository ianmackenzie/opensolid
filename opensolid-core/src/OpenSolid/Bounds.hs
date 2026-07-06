module OpenSolid.Bounds
  ( Bounds
  , Exists
  , constant
  , contains
  , hull2
  , aggregate2
  , intersection
  , diameter
  , transformBy
  )
where

import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Set qualified as Set
import OpenSolid.Transform (Transform)
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds

type family Bounds dimension units space = bounds | bounds -> dimension units space where
  Bounds 2 units Void = Bounds2D units
  Bounds 3 Meters space = Bounds3D space

class
  ( Point.Exists dimension units space
  , VectorBounds.Exists dimension units space
  , Show (Bounds dimension units space)
  , Set.Bounds (Bounds dimension units space)
  , Intersects (Point dimension units space) (Bounds dimension units space) (Tolerance units)
  , Intersects (Bounds dimension units space) (Point dimension units space) (Tolerance units)
  , Intersects (Bounds dimension units space) (Bounds dimension units space) (Tolerance units)
  , Addition (Bounds dimension units space) (VectorBounds dimension units space) (Bounds dimension units space)
  , Addition (Point dimension units space) (VectorBounds dimension units space) (Bounds dimension units space)
  , Subtraction (Bounds dimension units space) (VectorBounds dimension units space) (Bounds dimension units space)
  , Subtraction (Point dimension units space) (VectorBounds dimension units space) (Bounds dimension units space)
  , Subtraction (Bounds dimension units space) (Bounds dimension units space) (VectorBounds dimension units space)
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
  diameter :: Bounds dimension units space -> Quantity units
  transformBy ::
    Transform dimension tag units space ->
    Bounds dimension units space ->
    Bounds dimension units space

instance Exists 2 units Void where
  constant = Bounds2D.constant
  contains = Bounds2D.contains
  hull2 = Bounds2D.hull2
  aggregate2 = Bounds2D.aggregate2
  intersection = Bounds2D.intersection
  diameter = Bounds2D.diameter
  transformBy = Bounds2D.transformBy

instance Exists 3 Meters space where
  constant = Bounds3D.constant
  contains = Bounds3D.contains
  hull2 = Bounds3D.hull2
  aggregate2 = Bounds3D.aggregate2
  intersection = Bounds3D.intersection
  diameter = Bounds3D.diameter
  transformBy = Bounds3D.transformBy
