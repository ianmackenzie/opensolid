module OpenSolid.Direction2D
  ( Direction2D (Direction2D)
  , xComponent
  , yComponent
  , components
  , unsafe
  , unwrap
  , x
  , y
  , PointsAreCoincident (PointsAreCoincident)
  , from
  , fromAngle
  , angle
  , degrees
  , radians
  , angleFrom
  , parallel
  , perpendicular
  , perpendicularTo
  , rotateLeft
  , rotateRight
  , placeIn
  , placeInOrientation
  , relativeTo
  , relativeToOrientation
  , placeOn
  , placeOnOrientation
  , random
  , transformBy
  , rotateBy
  , mirrorIn
  , mirrorAcross
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2D
  , Direction2D (Unit2D)
  , Direction3D (Unit3D)
  , Frame2D
  , Orientation2D
  , Plane3D
  , PlaneOrientation3D
  , Point2D
  , Transform2D
  , Vector2D (Vector2D)
  )
import OpenSolid.Primitives qualified as Primitives
import OpenSolid.Random qualified as Random
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector2D qualified as Vector2D

{-# COMPLETE Direction2D #-}

{-# INLINE Direction2D #-}
pattern Direction2D :: Number -> Number -> Direction2D
pattern Direction2D dx dy <- Unit2D (Vector2D dx dy)

-- | Get the X component of a direction.
xComponent :: Direction2D -> Number
xComponent (Direction2D dx _) = dx

-- | Get the Y component of a direction.
yComponent :: Direction2D -> Number
yComponent (Direction2D _ dy) = dy

-- | Get the XY components of a direction as a tuple.
{-# INLINE components #-}
components :: Direction2D -> (Number, Number)
components (Direction2D dx dy) = (dx, dy)

{-# INLINE unsafe #-}
unsafe :: Vector2D Unitless -> Direction2D
unsafe = Unit2D

{-# INLINE unwrap #-}
unwrap :: Direction2D -> Vector2D Unitless
unwrap (Unit2D vector) = vector

{-# INLINE lift #-}
lift :: (Vector2D Unitless -> Vector2D Unitless) -> Direction2D -> Direction2D
lift function (Unit2D vector) = Unit2D (function vector)

-- | The X direction.
x :: Direction2D
x = Unit2D (Vector2D 1.0 0.0)

-- | The Y direction.
y :: Direction2D
y = Unit2D (Vector2D 0.0 1.0)

data PointsAreCoincident = PointsAreCoincident deriving (Eq, Show)

from :: Tolerance units => Point2D units -> Point2D units -> Result PointsAreCoincident Direction2D
from p1 p2 = do
  case Vector2D.direction (p2 - p1) of
    Ok direction -> Ok direction
    Error Vector.IsZero -> Error PointsAreCoincident

{-| Construct a direction from an angle.

The angle is measured counterclockwise from the positive X direction, so:

  * An angle of zero corresponds to the positive X direction
  * An angle of 90 degrees corresponds to the positive Y direction
  * An angle of 180 degrees (or -180 degrees) corresponds to the negative X direction
-}
fromAngle :: Angle -> Direction2D
fromAngle theta = Unit2D (Vector2D (Angle.cos theta) (Angle.sin theta))

{-| Construct a direction from an angle given in degrees.

See 'fromAngle' for details.
-}
degrees :: Number -> Direction2D
degrees value = fromAngle (Angle.degrees value)

{-| Construct a direction from an angle given in radians.

See 'fromAngle' for details.
-}
radians :: Number -> Direction2D
radians value = fromAngle (Angle.radians value)

{-| Get the angle of a direction.

The angle is measured counterclockwise from the positive X direction, so:

  * The positive X direction has an angle of zero.
  * The positive Y direction has an angle of 90 degrees.
  * The negative Y direction has an angle of -90 degrees.
  * It is not defined whether the negative X direction has an angle of -180 or
    +180 degrees. (Currently it is reported as having an angle of +180 degrees,
    but this should not be relied upon.)

The returned angle will be between -180 and +180 degrees.
-}
angle :: Direction2D -> Angle
angle (Unit2D vector) = Vector2D.angle vector

{-| Measure the signed angle from one direction to another.

The angle will be measured counterclockwise from the first direction to the
second, and will always be between -180 and +180 degrees.
-}
angleFrom :: Direction2D -> Direction2D -> Angle
angleFrom (Unit2D v1) (Unit2D v2) = Vector2D.angleFrom v1 v2

perpendicular :: Direction2D -> Direction2D -> Bool
perpendicular d1 d2 = Tolerance.using Tolerance.unitless (d1 `dot` d2 ~= 0.0)

parallel :: Direction2D -> Direction2D -> Bool
parallel d1 d2 = d1 ~= d2 || d1 ~= -d2

perpendicularTo :: Direction2D -> Direction2D
perpendicularTo = rotateLeft

-- | Rotate a direction left (counterclockwise) by 90 degrees.
rotateLeft :: Direction2D -> Direction2D
rotateLeft = lift Vector2D.rotateLeft

-- | Rotate a direction right (clockwise) by 90 degrees.
rotateRight :: Direction2D -> Direction2D
rotateRight = lift Vector2D.rotateRight

placeIn :: Frame2D frameUnits -> Direction2D -> Direction2D
placeIn frame = placeInOrientation frame.orientation

placeInOrientation :: Orientation2D -> Direction2D -> Direction2D
placeInOrientation orientation = lift (Vector2D.placeInOrientation orientation)

relativeTo :: Frame2D frameUnits -> Direction2D -> Direction2D
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation :: Orientation2D -> Direction2D -> Direction2D
relativeToOrientation orientation = lift (Vector2D.relativeToOrientation orientation)

{-| Convert a 2D direction to 3D direction by placing it on a plane.

Given a 2D direction defined within a plane's coordinate system,
this returns the corresponding 3D direction.
-}
placeOn :: Plane3D space -> Direction2D -> Direction3D space
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation :: PlaneOrientation3D space -> Direction2D -> Direction3D space
placeOnOrientation orientation (Unit2D vector) =
  Unit3D (Vector2D.placeOnOrientation orientation vector)

random :: Random.Generator Direction2D
random = Random.map fromAngle (Random.quantity -Angle.pi Angle.pi)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform2D tag translationUnits ->
  Direction2D ->
  Direction2D
transformBy transform = lift (Vector2D.transformBy transform)

{-| Rotate a direction by a given angle.

A positive angle corresponds to a counterclockwise rotation.
-}
rotateBy :: Angle -> Direction2D -> Direction2D
rotateBy theta = lift (Vector2D.rotateBy theta)

{-| Mirror a direction in/along a given other direction.

For example, mirroring in the X direction
will negate the original direction's X component and leave its Y component unchanged.
-}
mirrorIn :: Direction2D -> Direction2D -> Direction2D
mirrorIn mirrorDirection = lift (Vector2D.mirrorIn mirrorDirection)

{-| Mirror a direction across a given axis.

The origin point of the axis is not used, only its direction, since directions have no position.
For example, mirroring a direction across *any* axis parallel to the X axis
will negate the direction's Y component while leaving its X component unchanged.
-}
mirrorAcross :: Axis2D originUnits -> Direction2D -> Direction2D
mirrorAcross axis = lift (Vector2D.mirrorAcross axis)
