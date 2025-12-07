module OpenSolid.Vector2D
  ( Vector2D
  , pattern Vector2D
  , zero
  , x
  , y
  , from
  , meters
  , centimeters
  , cm
  , millimeters
  , mm
  , inches
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
  , sum
  , transformBy
  , rotateBy
  , mirrorIn
  , mirrorAcross
  , scaleIn
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Area (Area)
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Orientation2d (Orientation2d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.PlaneOrientation3d (PlaneOrientation3d)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Polymorphic.Vector2d (IsZero (IsZero), Vector2d (Vector2d))
import OpenSolid.Polymorphic.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d)

type Vector2D space = Vector2d Meters space

{-# COMPLETE Vector2D #-}

-- | Construct a vector from its X and Y components.
{-# INLINE Vector2D #-}
pattern Vector2D :: Length -> Length -> Vector2D space
pattern Vector2D vx vy = Vector2d vx vy

-- | The zero vector.
zero :: Vector2D space
zero = Vector2d.zero

{-| Construct a vector from just an X component.

The Y component will be set to zero.
-}
x :: Length -> Vector2D space
x = Vector2d.x

{-| Construct a vector from just a Y component.

The X component will be set to zero.
-}
y :: Length -> Vector2D space
y = Vector2d.y

from :: Point2D space -> Point2D space -> Vector2D space
from = Vector2d.from

apply :: (Number -> Length) -> Number -> Number -> Vector2D space
apply units px py = Vector2D (units px) (units py)

-- | Construct a vector from its X and Y components given in meters.
meters :: Number -> Number -> Vector2d Meters space
meters = apply Length.meters

-- | Construct a vector from its X and Y components given in centimeters.
centimeters :: Number -> Number -> Vector2d Meters space
centimeters = apply Length.centimeters

{-| Construct a vector from its X and Y components given in centimeters.

Short form alias for 'centimeters'.
-}
cm :: Number -> Number -> Vector2d Meters space
cm = centimeters

-- | Construct a vector from its X and Y components given in millimeters.
millimeters :: Number -> Number -> Vector2d Meters space
millimeters = apply Length.millimeters

{-| Construct a vector from its X and Y components given in millimeters.

Short form alias for 'millimeters'.
-}
mm :: Number -> Number -> Vector2d Meters space
mm = millimeters

-- | Construct a vector from its X and Y components given in inches.
inches :: Number -> Number -> Vector2d Meters space
inches = apply Length.inches

-- | Construct a vector from its magnitude (length) and angle.
polar :: Length -> Angle -> Vector2D space
polar = Vector2d.polar

-- | Get the X component of a vector.
xComponent :: Vector2D space -> Length
xComponent = Vector2d.xComponent

-- | Get the Y component of a vector.
yComponent :: Vector2D space -> Length
yComponent = Vector2d.yComponent

componentIn :: Direction2d space -> Vector2D space -> Length
componentIn = Vector2d.componentIn

projectionIn :: Direction2d space -> Vector2D space -> Vector2D space
projectionIn = Vector2d.projectionIn

-- | Get the X and Y components of a vector as a tuple.
{-# INLINE components #-}
components :: Vector2D space -> (Length, Length)
components = Vector2d.components

interpolateFrom :: Vector2D space -> Vector2D space -> Number -> Vector2D space
interpolateFrom = Vector2d.interpolateFrom

midpoint :: Vector2D space -> Vector2D space -> Vector2D space
midpoint = Vector2d.midpoint

magnitude :: Vector2D space -> Length
magnitude = Vector2d.magnitude

squaredMagnitude :: Vector2D space -> Area
squaredMagnitude = Vector2d.squaredMagnitude

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
angle :: Vector2D space -> Angle
angle = Vector2d.angle

{-| Measure the signed angle from one vector to another.

The angle will be measured counterclockwise from the first vector to the
second, and will always be between -180 and +180 degrees.
-}
angleFrom :: Vector2D space -> Vector2D space -> Angle
angleFrom = Vector2d.angleFrom

{-| Attempt to get the direction of a vector.

The current tolerance will be used to check if the vector is zero
(and therefore does not have a direction).
-}
direction :: Tolerance Meters => Vector2D space -> Result IsZero (Direction2d space)
direction = Vector2d.direction

magnitudeAndDirection ::
  Tolerance Meters =>
  Vector2D space ->
  Result IsZero (Length, Direction2d space)
magnitudeAndDirection = Vector2d.magnitudeAndDirection

{-| Normalize a vector.

If the original vector is exactly zero, then the result will be zero as well.
Otherwise, the result will be a unit vector.
-}
normalize :: Vector2D space -> Vector2d Unitless space
normalize = Vector2d.normalize

-- | Rotate a vector left (counterclockwise) by 90 degrees.
rotateLeft :: Vector2D space -> Vector2D space
rotateLeft = Vector2d.rotateLeft

-- | Rotate a vector right (clockwise) by 90 degrees.
rotateRight :: Vector2D space -> Vector2D space
rotateRight = Vector2d.rotateRight

{-# INLINE placeIn #-}
placeIn :: Frame2d Meters global local -> Vector2D local -> Vector2D global
placeIn = Vector2d.placeIn

placeInOrientation :: Orientation2d global -> Vector2D local -> Vector2D global
placeInOrientation = Vector2d.placeInOrientation

{-# INLINE relativeTo #-}
relativeTo :: Frame2d frameUnits global local -> Vector2D global -> Vector2D local
relativeTo = Vector2d.relativeTo

relativeToOrientation :: Orientation2d global -> Vector2D global -> Vector2D local
relativeToOrientation = Vector2d.relativeToOrientation

{-| Convert a 2D vector to 3D vector by placing it on a plane.

Given a 2D vector defined within a plane's coordinate system,
this returns the corresponding 3D vector.
-}
{-# INLINE placeOn #-}
placeOn :: Plane3d global local -> Vector2D local -> Vector3d Meters global
placeOn = Vector2d.placeOn

placeOnOrientation :: PlaneOrientation3d global -> Vector2D local -> Vector3d Meters global
placeOnOrientation = Vector2d.placeOnOrientation

sum :: List (Vector2D space) -> Vector2D space
sum = List.foldl (.+.) zero

transformBy :: Transform2d tag Meters space -> Vector2D space -> Vector2D space
transformBy = Vector2d.transformBy

{-| Rotate a vector by a given angle.

A positive angle corresponds to a counterclockwise rotation.
-}
rotateBy :: Angle -> Vector2D space -> Vector2D space
rotateBy = Vector2d.rotateBy

{-| Mirror a vector in/along a given direction.

For example, mirroring in the X direction
will negate the vector's X component and leave its Y component unchanged.
-}
mirrorIn :: Direction2d space -> Vector2D space -> Vector2D space
mirrorIn = Vector2d.mirrorIn

{-| Mirror a vector across a given axis.

The origin point of the axis is not used, only its direction, since vectors have no position.
For example, mirroring a vector across *any* axis parallel to the X axis
will negate the vector's Y component while leaving its X component unchanged.
-}
mirrorAcross :: Axis2d Meters space -> Vector2D space -> Vector2D space
mirrorAcross = Vector2d.mirrorAcross

scaleIn :: Direction2d space -> Number -> Vector2D space -> Vector2D space
scaleIn = Vector2d.scaleIn
