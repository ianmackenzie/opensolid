module OpenSolid.Circle2D
  ( Circle2D (Circle2D)
  , with
  , centerPoint
  , radius
  , diameter
  , point
  , bounds
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Circle3D (Circle3D)
import OpenSolid.Circle3D qualified as Circle3D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vector2D qualified as Vector2D

-- | A circle in 2D.
data Circle2D units = Circle2D
  { centerPoint :: Point2D units
  , radius :: Quantity units
  }
  deriving (Show)

instance FFI (Circle2D Meters) where
  representation = FFI.classRepresentation "Circle2D"

instance FFI (Circle2D Unitless) where
  representation = FFI.classRepresentation "UvCircle"

instance units ~ Meters => On (Plane3D space) (Circle2D units) (Circle3D space) where
  on plane circle = do
    let originPoint = on plane (centerPoint circle)
    Circle3D.with
      { plane = Plane3D.moveTo originPoint plane
      , diameter = diameter circle
      }

{-| Construct a point on the circle, at the given angle.

The angle is measured counterclockwise from the positive X direction.
-}
instance At (Circle2D units) Angle (Point2D units) where
  circle @ angle = centerPoint circle + Vector2D.polar (radius circle) angle

{-| Construct a point on the circle, given a parameter that ranges from 0 to 1.

'on circle t' is equivalent to 'on circle (Angle.turns t)'.
-}
instance At (Circle2D units) Number (Point2D units) where
  circle @ parameterValue = circle @ Angle.turns parameterValue

-- | Construct a circle with the given center point and diameter.
with :: ("centerPoint" ::: Point2D units, "diameter" ::: Quantity units) -> Circle2D units
with params
  | params.diameter < Quantity.zero = error "Diameter must not be negative"
  | otherwise = Circle2D {centerPoint = params.centerPoint, radius = 0.5 * params.diameter}

-- | Get the center point of a circle.
centerPoint :: Circle2D units -> Point2D units
centerPoint = (.centerPoint)

-- | Get the radius of a circle.
radius :: Circle2D units -> Quantity units
radius = (.radius)

-- | Get the diameter of a circle.
diameter :: Circle2D units -> Quantity units
diameter circle = 2.0 * radius circle

{-| Construct a point on the circle, at the given angle.

The angle is measured counterclockwise from the positive X direction.
-}
point :: Circle2D units -> Angle -> Point2D units
point circle angle = centerPoint circle + Vector2D.polar (radius circle) angle

bounds :: Circle2D units -> Bounds2D units
bounds circle = do
  let Point2D cx cy = centerPoint circle
  let r = radius circle
  let xInterval = Interval (cx - r) (cx + r)
  let yInterval = Interval (cy - r) (cy + r)
  Bounds2D xInterval yInterval
