module OpenSolid.Direction2D
  ( Direction2D (Direction2D)
  , xComponent
  , yComponent
  , components
  , unsafe
  , unwrap
  , coerce
  , x
  , y
  , PointsAreCoincident (PointsAreCoincident)
  , from
  , fromAngle
  , angle
  , degrees
  , radians
  , angleFrom
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
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random qualified as Random
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector2D qualified as Vector2D

{-# COMPLETE Direction2D #-}

{-# INLINE Direction2D #-}
pattern Direction2D :: Number -> Number -> Direction2D space
pattern Direction2D dx dy <- Unit2D (Vector2D dx dy)

-- | Get the X component of a direction.
xComponent :: Direction2D space -> Number
xComponent (Direction2D dx _) = dx

-- | Get the Y component of a direction.
yComponent :: Direction2D space -> Number
yComponent (Direction2D _ dy) = dy

-- | Get the XY components of a direction as a tuple.
{-# INLINE components #-}
components :: Direction2D space -> (Number, Number)
components (Direction2D dx dy) = (dx, dy)

{-# INLINE unsafe #-}
unsafe :: Vector2D Unitless space -> Direction2D space
unsafe = Unit2D

{-# INLINE unwrap #-}
unwrap :: Direction2D space -> Vector2D Unitless space
unwrap (Unit2D vector) = vector

{-# INLINE coerce #-}
coerce :: Direction2D space1 -> Direction2D space2
coerce (Unit2D (Vector2D dx dy)) = Unit2D (Vector2D dx dy)

{-# INLINE lift #-}
lift ::
  (Vector2D Unitless space1 -> Vector2D Unitless space2) ->
  Direction2D space1 ->
  Direction2D space2
lift function (Unit2D vector) = Unit2D (function vector)

-- | The X direction.
x :: Direction2D space
x = Unit2D (Vector2D 1 0)

-- | The Y direction.
y :: Direction2D space
y = Unit2D (Vector2D 0 1)

data PointsAreCoincident = PointsAreCoincident deriving (Eq, Show)

from ::
  Tolerance units =>
  Point2D units space ->
  Point2D units space ->
  Result PointsAreCoincident (Direction2D space)
from p1 p2 = do
  case Vector2D.direction (p2 .-. p1) of
    Ok direction -> Ok direction
    Error Vector.IsZero -> Error PointsAreCoincident

{-| Construct a direction from an angle.

The angle is measured counterclockwise from the positive X direction, so:

  * An angle of zero corresponds to the positive X direction
  * An angle of 90 degrees corresponds to the positive Y direction
  * An angle of 180 degrees (or -180 degrees) corresponds to the negative X direction
-}
fromAngle :: Angle -> Direction2D space
fromAngle theta = Unit2D (Vector2D (Angle.cos theta) (Angle.sin theta))

{-| Construct a direction from an angle given in degrees.

See 'fromAngle' for details.
-}
degrees :: Number -> Direction2D space
degrees value = fromAngle (Angle.degrees value)

{-| Construct a direction from an angle given in radians.

See 'fromAngle' for details.
-}
radians :: Number -> Direction2D space
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
angle :: Direction2D space -> Angle
angle (Unit2D vector) = Vector2D.angle vector

{-| Measure the signed angle from one direction to another.

The angle will be measured counterclockwise from the first direction to the
second, and will always be between -180 and +180 degrees.
-}
angleFrom :: Direction2D space -> Direction2D space -> Angle
angleFrom (Unit2D v1) (Unit2D v2) = Vector2D.angleFrom v1 v2

perpendicularTo :: Direction2D space -> Direction2D space
perpendicularTo = rotateLeft

-- | Rotate a direction left (counterclockwise) by 90 degrees.
rotateLeft :: Direction2D space -> Direction2D space
rotateLeft = lift Vector2D.rotateLeft

-- | Rotate a direction right (clockwise) by 90 degrees.
rotateRight :: Direction2D space -> Direction2D space
rotateRight = lift Vector2D.rotateRight

placeIn :: Frame2D frameUnits global local -> Direction2D local -> Direction2D global
placeIn frame = placeInOrientation frame.orientation

placeInOrientation :: Orientation2D global -> Direction2D local -> Direction2D global
placeInOrientation orientation = lift (Vector2D.placeInOrientation orientation)

relativeTo :: Frame2D frameUnits global local -> Direction2D global -> Direction2D local
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation :: Orientation2D global -> Direction2D global -> Direction2D local
relativeToOrientation orientation = lift (Vector2D.relativeToOrientation orientation)

{-| Convert a 2D direction to 3D direction by placing it on a plane.

Given a 2D direction defined within a plane's coordinate system,
this returns the corresponding 3D direction.
-}
placeOn :: Plane3D global local -> Direction2D local -> Direction3D global
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation :: PlaneOrientation3D global -> Direction2D local -> Direction3D global
placeOnOrientation orientation (Unit2D vector) =
  Unit3D (Vector2D.placeOnOrientation orientation vector)

random :: Random.Generator (Direction2D space)
random = Random.map fromAngle (Quantity.random (negative Angle.pi) Angle.pi)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform2D tag translationUnits space ->
  Direction2D space ->
  Direction2D space
transformBy transform = lift (Vector2D.transformBy transform)

{-| Rotate a direction by a given angle.

A positive angle corresponds to a counterclockwise rotation.
-}
rotateBy :: Angle -> Direction2D space -> Direction2D space
rotateBy theta = lift (Vector2D.rotateBy theta)

{-| Mirror a direction in/along a given other direction.

For example, mirroring in the X direction
will negate the original direction's X component and leave its Y component unchanged.
-}
mirrorIn :: Direction2D space -> Direction2D space -> Direction2D space
mirrorIn mirrorDirection = lift (Vector2D.mirrorIn mirrorDirection)

{-| Mirror a direction across a given axis.

The origin point of the axis is not used, only its direction, since directions have no position.
For example, mirroring a direction across *any* axis parallel to the X axis
will negate the direction's Y component while leaving its X component unchanged.
-}
mirrorAcross ::
  forall originUnits space.
  Axis2D originUnits space ->
  Direction2D space ->
  Direction2D space
mirrorAcross axis = lift (Vector2D.mirrorAcross axis)
