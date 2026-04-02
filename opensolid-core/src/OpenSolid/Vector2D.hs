module OpenSolid.Vector2D
  ( Vector2D (Vector2D, Vector2D#)
  , zero
  , coerce
  , unit
  , x
  , y
  , from
  , meters
  , centimeters
  , cm
  , millimeters
  , mm
  , inches
  , squareMeters
  , polar
  , xComponent
  , yComponent
  , componentIn
  , projectionIn
  , components
  , midpoint
  , interpolateFrom
  , magnitude
  , squaredMagnitude
  , squaredMagnitude_
  , angle
  , angleFrom
  , direction
  , magnitudeAndDirection
  , normalize
  , rotateRight
  , rotateLeft
  , placeIn
  , placeInOrientation
  , relativeTo
  , relativeToOrientation
  , placeOn
  , placeOnOrientation
  , convert
  , unconvert
  , sum
  , transformBy
  , rotateBy
  , mirrorIn
  , mirrorAcross
  , scaleIn
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Area qualified as Area
import {-# SOURCE #-} OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2D (Axis2D)
  , Direction2D (Unit2D)
  , Frame2D
  , Orientation2D (Orientation2D)
  , Plane3D
  , PlaneOrientation3D (PlaneOrientation3D)
  , Point2D
  , Transform2D (Transform2D)
  , Vector2D (Vector2D, Vector2D#)
  , Vector3D
  )
import OpenSolid.Primitives qualified as Primitives
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units (SquareMeters)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector qualified as Vector

-- | The zero vector.
zero :: Vector2D units
zero = Vector.zero

{-# INLINE coerce #-}
coerce :: Vector2D units1 -> Vector2D units2
coerce (Vector2D vx vy) = Vector2D (Quantity.coerce vx) (Quantity.coerce vy)

-- | Construct a unit vector in the given direction.
{-# INLINE unit #-}
unit :: Direction2D -> Vector2D Unitless
unit (Unit2D vector) = vector

{-| Construct a vector from just an X component.

The Y component will be set to zero.
-}
x :: Quantity units -> Vector2D units
x vx = Vector2D vx Quantity.zero

{-| Construct a vector from just a Y component.

The X component will be set to zero.
-}
y :: Quantity units -> Vector2D units
y vy = Vector2D Quantity.zero vy

from :: Point2D units -> Point2D units -> Vector2D units
from p1 p2 = p2 - p1

apply :: (Number -> Quantity units) -> Number -> Number -> Vector2D units
apply units px py = Vector2D (units px) (units py)

-- | Construct a vector from its X and Y components given in meters.
meters :: Number -> Number -> Vector2D Meters
meters = apply Length.meters

-- | Construct a vector from its X and Y components given in centimeters.
centimeters :: Number -> Number -> Vector2D Meters
centimeters = apply Length.centimeters

{-| Construct a vector from its X and Y components given in centimeters.

Short form alias for 'centimeters'.
-}
cm :: Number -> Number -> Vector2D Meters
cm = centimeters

-- | Construct a vector from its X and Y components given in millimeters.
millimeters :: Number -> Number -> Vector2D Meters
millimeters = apply Length.millimeters

{-| Construct a vector from its X and Y components given in millimeters.

Short form alias for 'millimeters'.
-}
mm :: Number -> Number -> Vector2D Meters
mm = millimeters

-- | Construct a vector from its X and Y components given in inches.
inches :: Number -> Number -> Vector2D Meters
inches = apply Length.inches

-- | Construct a vector from its X and Y components given in square meters.
squareMeters :: Number -> Number -> Vector2D SquareMeters
squareMeters = apply Area.squareMeters

-- | Construct a vector from its magnitude (length) and angle.
polar :: Quantity units -> Angle -> Vector2D units
polar r theta = Vector2D (r * Angle.cos theta) (r * Angle.sin theta)

-- | Get the X component of a vector.
xComponent :: Vector2D units -> Quantity units
xComponent (Vector2D vx _) = vx

-- | Get the Y component of a vector.
yComponent :: Vector2D units -> Quantity units
yComponent (Vector2D _ vy) = vy

componentIn :: Direction2D -> Vector2D units -> Quantity units
componentIn = Vector.componentIn

projectionIn :: Direction2D -> Vector2D units -> Vector2D units
projectionIn = Vector.projectionIn

-- | Get the X and Y components of a vector as a tuple.
{-# INLINE components #-}
components :: Vector2D units -> (Quantity units, Quantity units)
components (Vector2D vx vy) = (vx, vy)

interpolateFrom :: Vector2D units -> Vector2D units -> Number -> Vector2D units
interpolateFrom (Vector2D x1 y1) (Vector2D x2 y2) t =
  Vector2D (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))

midpoint :: Vector2D units -> Vector2D units -> Vector2D units
midpoint (Vector2D x1 y1) (Vector2D x2 y2) =
  Vector2D (0.5 * (x1 + x2)) (0.5 * (y1 + y2))

magnitude :: Vector2D units -> Quantity units
magnitude = Vector.magnitude

squaredMagnitude :: Units.Squared units1 units2 => Vector2D units1 -> Quantity units2
squaredMagnitude = Vector.squaredMagnitude

squaredMagnitude_ :: Vector2D units -> Quantity (units ?*? units)
squaredMagnitude_ = Vector.squaredMagnitude_

{-| Get the angle of a vector.

The angle is measured counterclockwise from the positive X axis, so:

  * A vector in the positive X direction has an angle of zero.
  * A vector in the positive Y direction has an angle of 90 degrees.
  * A vector in the negative Y direction has an angle of -90 degrees.
  * It is not defined whether a vector exactly in the negative X direction has
    an angle of -180 or +180 degrees. (Currently it is reported as having an
    angle of +180 degrees, but this should not be relied upon.)

The returned angle will be between -180 and +180 degrees.
-}
angle :: Vector2D units -> Angle
angle (Vector2D vx vy) = Angle.atan2 vy vx

{-| Measure the signed angle from one vector to another.

The angle will be measured counterclockwise from the first vector to the
second, and will always be between -180 and +180 degrees.
-}
angleFrom :: Vector2D units -> Vector2D units -> Angle
angleFrom v1 v2 = Angle.atan2 (v1 `cross_` v2) (v1 `dot_` v2)

{-| Attempt to get the direction of a vector.

The current tolerance will be used to check if the vector is zero
(and therefore does not have a direction).
-}
direction :: Tolerance units => Vector2D units -> Result Vector.IsZero Direction2D
direction = Vector.direction

magnitudeAndDirection ::
  Tolerance units =>
  Vector2D units ->
  Result Vector.IsZero (Quantity units, Direction2D)
magnitudeAndDirection = Vector.magnitudeAndDirection

{-| Normalize a vector.

If the original vector is zero (within the current tolerance), then the result will be zero as well.
Otherwise, the result will be a unit vector.
-}
normalize :: Tolerance units => Vector2D units -> Vector2D Unitless
normalize = Vector.normalize

-- | Rotate a vector left (counterclockwise) by 90 degrees.
rotateLeft :: Vector2D units -> Vector2D units
rotateLeft (Vector2D vx vy) = Vector2D -vy vx

-- | Rotate a vector right (clockwise) by 90 degrees.
rotateRight :: Vector2D units -> Vector2D units
rotateRight (Vector2D vx vy) = Vector2D vy -vx

{-# INLINE placeIn #-}
placeIn :: Frame2D frameUnits -> Vector2D units -> Vector2D units
placeIn frame = placeInOrientation frame.orientation

placeInOrientation :: Orientation2D -> Vector2D units -> Vector2D units
placeInOrientation (Orientation2D i j) (Vector2D vx vy) = vx * i + vy * j

{-# INLINE relativeTo #-}
relativeTo :: Frame2D frameUnits -> Vector2D units -> Vector2D units
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation :: Orientation2D -> Vector2D units -> Vector2D units
relativeToOrientation (Orientation2D i j) vector = Vector2D (vector `dot` i) (vector `dot` j)

{-| Convert a 2D vector to 3D vector by placing it on a plane.

Given a 2D vector defined within a plane's coordinate system,
this returns the corresponding 3D vector.
-}
{-# INLINE placeOn #-}
placeOn :: Plane3D space -> Vector2D units -> Vector3D units space
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation :: PlaneOrientation3D space -> Vector2D units -> Vector3D units space
placeOnOrientation (PlaneOrientation3D i j) (Vector2D vx vy) = vx * i + vy * j

convert :: Quantity (units2 ?/? units1) -> Vector2D units1 -> Vector2D units2
convert factor vector = Units.simplify (vector ?*? factor)

unconvert :: Quantity (units2 ?/? units1) -> Vector2D units2 -> Vector2D units1
unconvert factor vector = Units.simplify (vector ?/? factor)

sum :: List (Vector2D units) -> Vector2D units
sum = Vector.sum

transformBy :: Transform2D tag translationUnits -> Vector2D units -> Vector2D units
transformBy transform vector = do
  let Transform2D _ i j = transform
  let Vector2D vx vy = vector
  vx * i + vy * j

{-| Rotate a vector by a given angle.

A positive angle corresponds to a counterclockwise rotation.
-}
rotateBy :: Angle -> Vector2D units -> Vector2D units
rotateBy theta (Vector2D vx vy) = do
  let cosTheta = Angle.cos theta
  let sinTheta = Angle.sin theta
  Vector2D (cosTheta * vx - sinTheta * vy) (sinTheta * vx + cosTheta * vy)

{-| Mirror a vector in/along a given direction.

For example, mirroring in the X direction
will negate the vector's X component and leave its Y component unchanged.
-}
mirrorIn :: Direction2D -> Vector2D units -> Vector2D units
mirrorIn mirrorDirection vector = vector - 2.0 * projectionIn mirrorDirection vector

{-| Mirror a vector across a given axis.

The origin point of the axis is not used, only its direction, since vectors have no position.
For example, mirroring a vector across *any* axis parallel to the X axis
will negate the vector's Y component while leaving its X component unchanged.
-}
mirrorAcross :: Axis2D originUnits -> Vector2D units -> Vector2D units
mirrorAcross (Axis2D _ axisDirection) = mirrorIn (Direction2D.rotateLeft axisDirection)

scaleIn :: Direction2D -> Number -> Vector2D units -> Vector2D units
scaleIn scaleDirection scale vector =
  vector + (scale - 1.0) * projectionIn scaleDirection vector
