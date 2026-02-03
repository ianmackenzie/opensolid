module OpenSolid.Point
  ( Point
  )
where

import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)

type family Point dimension units space where
  Point 2 units space = Point2D units space
  Point 3 Meters space = Point3D space

class
  ( Addition
      (Point dimension units space)
      (Vector dimension units space)
      (Point dimension units space)
  , Subtraction
      (Point dimension units space)
      (Point dimension units space)
      (Vector dimension units space)
  ) =>
  Exists dimension units space

instance Exists 2 units space

instance Exists 3 Meters space
