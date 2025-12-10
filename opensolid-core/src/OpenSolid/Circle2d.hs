module OpenSolid.Circle2d
  ( Circle2d
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
import OpenSolid.Polymorphic.Point2d (Point2d)
import OpenSolid.Polymorphic.Vector2d qualified as Vector2d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

-- | A circle in 2D.
data Circle2d units space = Circle2d
  { centerPoint :: Point2d units space
  , radius :: Quantity units
  }
  deriving (Show)

instance FFI (Circle2d Meters FFI.Space) where
  representation = FFI.classRepresentation "Circle2d"

instance FFI (Circle2d Unitless UvSpace) where
  representation = FFI.classRepresentation "UvCircle"

-- | Get the center point of a circle.
centerPoint :: Circle2d units space -> Point2d units space
centerPoint = (.centerPoint)

-- | Get the radius of a circle.
radius :: Circle2d units space -> Quantity units
radius = (.radius)

-- | Get the diameter of a circle.
diameter :: Circle2d units space -> Quantity units
diameter circle = 2 *. radius circle

-- | Construct a circle with the given radius and center point.
withRadius :: Quantity units -> Point2d units space -> Circle2d units space
withRadius givenRadius givenCenterPoint =
  Circle2d
    { radius = Quantity.abs givenRadius
    , centerPoint = givenCenterPoint
    }

-- | Construct a circle with the given diameter and center point.
withDiameter :: Quantity units -> Point2d units space -> Circle2d units space
withDiameter givenDiameter = withRadius (0.5 *. givenDiameter)

{-| Construct a point on the circle, at the given angle.

The angle is measured counterclockwise from the positive X direction.
-}
pointOn :: Circle2d units space -> Angle -> Point2d units space
pointOn circle angle = centerPoint circle .+. Vector2d.polar (radius circle) angle
