module OpenSolid.Point
  ( Point
  , Exists
  , distanceFrom
  )
where

import Data.Void (Void)
import {-# SOURCE #-} OpenSolid.Bounds qualified as Bounds
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector

type family
  Point dimension units space =
    point | point -> dimension units space
  where
  Point 2 units Void = Point2D units
  Point 3 Meters space = Point3D space

class
  ( Vector.Exists dimension units space
  , Bounds.Exists dimension units space
  , Eq (Point dimension units space)
  , Show (Point dimension units space)
  , ApproximateEquality (Point dimension units space) (Tolerance units)
  , Addition
      (Point dimension units space)
      (Vector dimension units space)
      (Point dimension units space)
  , Subtraction
      (Point dimension units space)
      (Vector dimension units space)
      (Point dimension units space)
  , Subtraction
      (Point dimension units space)
      (Point dimension units space)
      (Vector dimension units space)
  ) =>
  Exists dimension units space
  where
  distanceFrom :: Point dimension units space -> Point dimension units space -> Quantity units

instance Exists 2 units Void where
  {-# INLINE distanceFrom #-}
  distanceFrom = Point2D.distanceFrom

instance Exists 3 Meters space where
  {-# INLINE distanceFrom #-}
  distanceFrom = Point3D.distanceFrom
