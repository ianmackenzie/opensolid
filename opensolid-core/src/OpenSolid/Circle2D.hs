module OpenSolid.Circle2D
  ( Circle2D
  , withRadius
  , withDiameter
  , centerPoint
  , radius
  , diameter
  , point
  , bounds
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Interval (Interval (Interval))
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

-- | Get the center point of a circle.
centerPoint :: Circle2D units -> Point2D units
centerPoint = (.centerPoint)

-- | Get the radius of a circle.
radius :: Circle2D units -> Quantity units
radius = (.radius)

-- | Get the diameter of a circle.
diameter :: Circle2D units -> Quantity units
diameter circle = 2.0 * radius circle

-- | Construct a circle with the given radius and center point.
withRadius :: Quantity units -> Point2D units -> Circle2D units
withRadius givenRadius givenCenterPoint =
  Circle2D
    { radius = Quantity.abs givenRadius
    , centerPoint = givenCenterPoint
    }

-- | Construct a circle with the given diameter and center point.
withDiameter :: Quantity units -> Point2D units -> Circle2D units
withDiameter givenDiameter = withRadius (0.5 * givenDiameter)

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
