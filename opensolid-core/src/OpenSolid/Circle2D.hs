module OpenSolid.Circle2D
  ( Circle2D
  , withRadius
  , withDiameter
  , centerPoint
  , radius
  , diameter
  , pointOn
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vector2D qualified as Vector2D

-- | A circle in 2D.
data Circle2D units space = Circle2D
  { centerPoint :: Point2D units space
  , radius :: Quantity units
  }
  deriving (Show)

instance FFI (Circle2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Circle2D"

instance FFI (Circle2D Unitless UvSpace) where
  representation = FFI.classRepresentation "UvCircle"

-- | Get the center point of a circle.
centerPoint :: Circle2D units space -> Point2D units space
centerPoint = (.centerPoint)

-- | Get the radius of a circle.
radius :: Circle2D units space -> Quantity units
radius = (.radius)

-- | Get the diameter of a circle.
diameter :: Circle2D units space -> Quantity units
diameter circle = 2 *. radius circle

-- | Construct a circle with the given radius and center point.
withRadius :: Quantity units -> Point2D units space -> Circle2D units space
withRadius givenRadius givenCenterPoint =
  Circle2D
    { radius = Quantity.abs givenRadius
    , centerPoint = givenCenterPoint
    }

-- | Construct a circle with the given diameter and center point.
withDiameter :: Quantity units -> Point2D units space -> Circle2D units space
withDiameter givenDiameter = withRadius (0.5 *. givenDiameter)

{-| Construct a point on the circle, at the given angle.

The angle is measured counterclockwise from the positive X direction.
-}
pointOn :: Circle2D units space -> Angle -> Point2D units space
pointOn circle angle = centerPoint circle .+. Vector2D.polar (radius circle) angle
