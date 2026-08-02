module OpenSolid.Axis
  ( Axis
  , AxisExists
  , originPoint
  , direction
  )
where

import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Axis3D (Axis3D)
import OpenSolid.Axis3D qualified as Axis3D
import OpenSolid.Direction (Direction, DirectionExists)
import OpenSolid.Point (Point, PointExists)
import OpenSolid.Prelude

type family
  Axis dimension units space =
    axis | axis -> dimension units space
  where
  Axis 2 units Void = Axis2D units
  Axis 3 Meters space = Axis3D space

class
  ( PointExists dimension units space
  , DirectionExists dimension space
  , Intersects (Point dimension units space) (Axis dimension units space) (Tolerance units)
  , Intersects (Axis dimension units space) (Point dimension units space) (Tolerance units)
  , Show (Axis dimension units space)
  ) =>
  AxisExists dimension units space
  where
  originPoint :: Axis dimension units space -> Point dimension units space
  direction :: Axis dimension units space -> Direction dimension space

instance AxisExists 2 units Void where
  originPoint = Axis2D.originPoint
  direction = Axis2D.direction

instance AxisExists 3 Meters space where
  originPoint = Axis3D.originPoint
  direction = Axis3D.direction
