module OpenSolid.Axis
  ( Axis
  , Exists
  , originPoint
  , direction
  )
where

import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Axis3D (Axis3D)
import OpenSolid.Axis3D qualified as Axis3D
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude

type family
  Axis dimension units space =
    axis | axis -> dimension units space
  where
  Axis 2 units space = Axis2D units space
  Axis 3 Meters space = Axis3D space

class
  ( Point.Exists dimension units space
  , Direction.Exists dimension space
  , Show (Axis dimension units space)
  ) =>
  Exists dimension units space
  where
  originPoint :: Axis dimension units space -> Point dimension units space
  direction :: Axis dimension units space -> Direction dimension space

instance Exists 2 units space where
  originPoint = Axis2D.originPoint
  direction = Axis2D.direction

instance Exists 3 Meters space where
  originPoint = Axis3D.originPoint
  direction = Axis3D.direction
