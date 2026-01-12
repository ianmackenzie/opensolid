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
  , IsZero (IsZero)
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
import OpenSolid.List qualified as List
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

-- | The zero vector.
zero :: Vector2D units space
zero = Vector2D Quantity.zero Quantity.zero

{-# INLINE coerce #-}
coerce :: Vector2D units1 space1 -> Vector2D units2 space2
coerce (Vector2D vx vy) = Vector2D (Quantity.coerce vx) (Quantity.coerce vy)

-- | Construct a unit vector in the given direction.
{-# INLINE unit #-}
unit :: Direction2D space -> Vector2D Unitless space
unit (Unit2D vector) = vector

{-| Construct a vector from just an X component.

The Y component will be set to zero.
-}
x :: Quantity units -> Vector2D units space
x vx = Vector2D vx Quantity.zero

{-| Construct a vector from just a Y component.

The X component will be set to zero.
-}
y :: Quantity units -> Vector2D units space
y vy = Vector2D Quantity.zero vy

from :: Point2D units space -> Point2D units space -> Vector2D units space
from p1 p2 = p2 .-. p1

apply :: (Number -> Quantity units) -> Number -> Number -> Vector2D units space
apply units px py = Vector2D (units px) (units py)

-- | Construct a vector from its X and Y components given in meters.
meters :: Number -> Number -> Vector2D Meters space
meters = apply Length.meters

-- | Construct a vector from its X and Y components given in centimeters.
centimeters :: Number -> Number -> Vector2D Meters space
centimeters = apply Length.centimeters

{-| Construct a vector from its X and Y components given in centimeters.

Short form alias for 'centimeters'.
-}
cm :: Number -> Number -> Vector2D Meters space
cm = centimeters

-- | Construct a vector from its X and Y components given in millimeters.
millimeters :: Number -> Number -> Vector2D Meters space
millimeters = apply Length.millimeters

{-| Construct a vector from its X and Y components given in millimeters.

Short form alias for 'millimeters'.
-}
mm :: Number -> Number -> Vector2D Meters space
mm = millimeters

-- | Construct a vector from its X and Y components given in inches.
inches :: Number -> Number -> Vector2D Meters space
inches = apply Length.inches

-- | Construct a vector from its X and Y components given in square meters.
squareMeters :: Number -> Number -> Vector2D SquareMeters space
squareMeters = apply Area.squareMeters

-- | Construct a vector from its magnitude (length) and angle.
polar :: Quantity units -> Angle -> Vector2D units space
polar r theta = Vector2D (r .*. Angle.cos theta) (r .*. Angle.sin theta)

-- | Get the X component of a vector.
xComponent :: Vector2D units space -> Quantity units
xComponent (Vector2D vx _) = vx

-- | Get the Y component of a vector.
yComponent :: Vector2D units space -> Quantity units
yComponent (Vector2D _ vy) = vy

componentIn :: Direction2D space -> Vector2D units space -> Quantity units
componentIn = dot

projectionIn :: Direction2D space -> Vector2D units space -> Vector2D units space
projectionIn givenDirection vector = givenDirection .*. componentIn givenDirection vector

-- | Get the X and Y components of a vector as a tuple.
{-# INLINE components #-}
components :: Vector2D units space -> (Quantity units, Quantity units)
components (Vector2D vx vy) = (vx, vy)

interpolateFrom ::
  Vector2D units space ->
  Vector2D units space ->
  Number ->
  Vector2D units space
interpolateFrom (Vector2D x1 y1) (Vector2D x2 y2) t =
  Vector2D (x1 .+. t .*. (x2 .-. x1)) (y1 .+. t .*. (y2 .-. y1))

midpoint :: Vector2D units space -> Vector2D units space -> Vector2D units space
midpoint (Vector2D x1 y1) (Vector2D x2 y2) =
  Vector2D (0.5 *. (x1 .+. x2)) (0.5 *. (y1 .+. y2))

magnitude :: Vector2D units space -> Quantity units
magnitude (Vector2D vx vy) = Quantity.hypot2 vx vy

squaredMagnitude :: Units.Squared units1 units2 => Vector2D units1 space -> Quantity units2
squaredMagnitude = Units.specialize . squaredMagnitude_

squaredMagnitude_ :: Vector2D units space -> Quantity (units ?*? units)
squaredMagnitude_ (Vector2D vx vy) = vx ?*? vx .+. vy ?*? vy

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
angle :: Vector2D units space -> Angle
angle (Vector2D vx vy) = Angle.atan2 vy vx

{-| Measure the signed angle from one vector to another.

The angle will be measured counterclockwise from the first vector to the
second, and will always be between -180 and +180 degrees.
-}
angleFrom :: Vector2D units space -> Vector2D units space -> Angle
angleFrom v1 v2 = Angle.atan2 (v1 `cross_` v2) (v1 `dot_` v2)

data IsZero = IsZero deriving (Eq, Show)

{-| Attempt to get the direction of a vector.

The current tolerance will be used to check if the vector is zero
(and therefore does not have a direction).
-}
direction :: Tolerance units => Vector2D units space -> Result IsZero (Direction2D space)
direction vector = do
  let vm = magnitude vector
  if vm ~= Quantity.zero then Error IsZero else Ok (Unit2D (vector ./. vm))

magnitudeAndDirection ::
  Tolerance units =>
  Vector2D units space ->
  Result IsZero (Quantity units, Direction2D space)
magnitudeAndDirection vector = do
  let vm = magnitude vector
  if vm ~= Quantity.zero then Error IsZero else Ok (vm, Unit2D (vector ./. vm))

{-| Normalize a vector.

If the original vector is exactly zero, then the result will be zero as well.
Otherwise, the result will be a unit vector.
-}
normalize :: Vector2D units space -> Vector2D Unitless space
normalize vector = do
  let vm = magnitude vector
  if vm == Quantity.zero then zero else vector ./. vm

-- | Rotate a vector left (counterclockwise) by 90 degrees.
rotateLeft :: Vector2D units space -> Vector2D units space
rotateLeft (Vector2D vx vy) = Vector2D (negative vy) vx

-- | Rotate a vector right (clockwise) by 90 degrees.
rotateRight :: Vector2D units space -> Vector2D units space
rotateRight (Vector2D vx vy) = Vector2D vy (negative vx)

{-# INLINE placeIn #-}
placeIn :: Frame2D frameUnits global local -> Vector2D units local -> Vector2D units global
placeIn frame = placeInOrientation frame.orientation

placeInOrientation ::
  Orientation2D global ->
  Vector2D units local ->
  Vector2D units global
placeInOrientation (Orientation2D i j) (Vector2D vx vy) = vx .*. i .+. vy .*. j

{-# INLINE relativeTo #-}
relativeTo :: Frame2D frameUnits global local -> Vector2D units global -> Vector2D units local
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation ::
  Orientation2D global ->
  Vector2D units global ->
  Vector2D units local
relativeToOrientation (Orientation2D i j) vector = Vector2D (vector `dot` i) (vector `dot` j)

{-| Convert a 2D vector to 3D vector by placing it on a plane.

Given a 2D vector defined within a plane's coordinate system,
this returns the corresponding 3D vector.
-}
{-# INLINE placeOn #-}
placeOn :: Plane3D global local -> Vector2D units local -> Vector3D units global
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation :: PlaneOrientation3D global -> Vector2D units local -> Vector3D units global
placeOnOrientation (PlaneOrientation3D i j) (Vector2D vx vy) = vx .*. i .+. vy .*. j

convert :: Quantity (units2 ?/? units1) -> Vector2D units1 space -> Vector2D units2 space
convert factor vector = Units.simplify (vector ?*? factor)

unconvert :: Quantity (units2 ?/? units1) -> Vector2D units2 space -> Vector2D units1 space
unconvert factor vector = Units.simplify (vector ?/? factor)

sum :: List (Vector2D units space) -> Vector2D units space
sum = List.foldl (.+.) zero

transformBy ::
  Transform2D tag translationUnits space ->
  Vector2D units space ->
  Vector2D units space
transformBy transform vector = do
  let Transform2D _ i j = transform
  let Vector2D vx vy = vector
  vx .*. i .+. vy .*. j

{-| Rotate a vector by a given angle.

A positive angle corresponds to a counterclockwise rotation.
-}
rotateBy :: Angle -> Vector2D units space -> Vector2D units space
rotateBy theta (Vector2D vx vy) = do
  let cosTheta = Angle.cos theta
  let sinTheta = Angle.sin theta
  Vector2D (cosTheta .*. vx .-. sinTheta .*. vy) (sinTheta .*. vx .+. cosTheta .*. vy)

{-| Mirror a vector in/along a given direction.

For example, mirroring in the X direction
will negate the vector's X component and leave its Y component unchanged.
-}
mirrorIn :: Direction2D space -> Vector2D units space -> Vector2D units space
mirrorIn mirrorDirection vector = vector .-. 2 *. projectionIn mirrorDirection vector

{-| Mirror a vector across a given axis.

The origin point of the axis is not used, only its direction, since vectors have no position.
For example, mirroring a vector across *any* axis parallel to the X axis
will negate the vector's Y component while leaving its X component unchanged.
-}
mirrorAcross ::
  Axis2D originUnits space ->
  Vector2D units space ->
  Vector2D units space
mirrorAcross (Axis2D _ axisDirection) = mirrorIn (Direction2D.rotateLeft axisDirection)

scaleIn :: Direction2D space -> Number -> Vector2D units space -> Vector2D units space
scaleIn scaleDirection scale vector =
  vector .+. (scale .- 1) .*. projectionIn scaleDirection vector
