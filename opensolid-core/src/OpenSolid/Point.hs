module OpenSolid.Point
  ( Point
  , Exists
  , distanceFrom
  , linearDeviation
  )
where

import {-# SOURCE #-} OpenSolid.Bounds qualified as Bounds
import OpenSolid.Line2D (Line2D (Line2D))
import OpenSolid.Line2D qualified as Line2D
import OpenSolid.Line3D (Line3D (Line3D))
import OpenSolid.Line3D qualified as Line3D
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
  Point 2 units space = Point2D units space
  Point 3 Meters space = Point3D space

class
  ( Vector.Exists dimension units space
  , Bounds.Exists dimension units space
  , ApproximateEquality (Point dimension units space) (Tolerance units)
  , Addition
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
  linearDeviation ::
    Point dimension units space ->
    Point dimension units space ->
    Point dimension units space ->
    Quantity units

instance Exists 2 units space where
  {-# INLINE distanceFrom #-}
  distanceFrom = Point2D.distanceFrom
  {-# INLINE linearDeviation #-}
  linearDeviation p1 p2 p0 = Line2D.distanceTo p0 (Line2D p1 p2)

instance Exists 3 Meters space where
  {-# INLINE distanceFrom #-}
  distanceFrom = Point3D.distanceFrom
  {-# INLINE linearDeviation #-}
  linearDeviation p1 p2 p0 = Line3D.distanceTo p0 (Line3D p1 p2)
