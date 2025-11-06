module OpenSolid.Vector2d
  ( Vector2d (Vector2d, Vector2d##)
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
  , squaredMagnitude#
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
import {-# SOURCE #-} OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Error qualified as Error
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Prelude hiding ((+), (-))
import OpenSolid.Primitives
  ( Axis2d (Axis2d)
  , Direction2d (Unit2d)
  , Frame2d
  , Orientation2d (Orientation2d)
  , Plane3d
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point2d
  , Transform2d (Transform2d)
  , Vector2d (Vector2d, Vector2d##)
  , Vector3d
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Units qualified as Units

-- | The zero vector.
zero :: Vector2d (space @ units)
zero = Vector2d Quantity.zero Quantity.zero

{-# INLINE coerce #-}
coerce :: Vector2d (space1 @ units1) -> Vector2d (space2 @ units2)
coerce (Vector2d vx vy) = Vector2d (Quantity.coerce vx) (Quantity.coerce vy)

-- | Construct a unit vector in the given direction.
{-# INLINE unit #-}
unit :: Direction2d space -> Vector2d (space @ Unitless)
unit (Unit2d vector) = vector

{-| Construct a vector from just an X component.

The Y component will be set to zero.
-}
x :: Quantity units -> Vector2d (space @ units)
x vx = Vector2d vx Quantity.zero

{-| Construct a vector from just a Y component.

The X component will be set to zero.
-}
y :: Quantity units -> Vector2d (space @ units)
y vy = Vector2d Quantity.zero vy

from :: Point2d (space @ units) -> Point2d (space @ units) -> Vector2d (space @ units)
from p1 p2 = p2 .-. p1

apply :: (Number -> Quantity units) -> Number -> Number -> Vector2d (space @ units)
apply units px py = Vector2d (units px) (units py)

-- | Construct a vector from its X and Y components given in meters.
meters :: Number -> Number -> Vector2d (space @ Meters)
meters = apply Length.meters

-- | Construct a vector from its X and Y components given in centimeters.
centimeters :: Number -> Number -> Vector2d (space @ Meters)
centimeters = apply Length.centimeters

{-| Construct a vector from its X and Y components given in centimeters.

Short form alias for 'centimeters'.
-}
cm :: Number -> Number -> Vector2d (space @ Meters)
cm = centimeters

-- | Construct a vector from its X and Y components given in millimeters.
millimeters :: Number -> Number -> Vector2d (space @ Meters)
millimeters = apply Length.millimeters

{-| Construct a vector from its X and Y components given in millimeters.

Short form alias for 'millimeters'.
-}
mm :: Number -> Number -> Vector2d (space @ Meters)
mm = millimeters

-- | Construct a vector from its X and Y components given in inches.
inches :: Number -> Number -> Vector2d (space @ Meters)
inches = apply Length.inches

-- | Construct a vector from its X and Y components given in square meters.
squareMeters :: Number -> Number -> Vector2d (space @ SquareMeters)
squareMeters = apply Area.squareMeters

-- | Construct a vector from its magnitude (length) and angle.
polar :: Quantity units -> Angle -> Vector2d (space @ units)
polar r theta = Vector2d (r .*. Angle.cos theta) (r .*. Angle.sin theta)

-- | Get the X component of a vector.
xComponent :: Vector2d (space @ units) -> Quantity units
xComponent (Vector2d vx _) = vx

-- | Get the Y component of a vector.
yComponent :: Vector2d (space @ units) -> Quantity units
yComponent (Vector2d _ vy) = vy

componentIn :: Direction2d space -> Vector2d (space @ units) -> Quantity units
componentIn = dot

projectionIn :: Direction2d space -> Vector2d (space @ units) -> Vector2d (space @ units)
projectionIn givenDirection vector = givenDirection .*. componentIn givenDirection vector

-- | Get the X and Y components of a vector as a tuple.
{-# INLINE components #-}
components :: Vector2d (space @ units) -> (Quantity units, Quantity units)
components (Vector2d vx vy) = (vx, vy)

interpolateFrom ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Number ->
  Vector2d (space @ units)
interpolateFrom (Vector2d x1 y1) (Vector2d x2 y2) t =
  Vector2d (x1 .+. t .*. (x2 .-. x1)) (y1 .+. t .*. (y2 .-. y1))

midpoint :: Vector2d (space @ units) -> Vector2d (space @ units) -> Vector2d (space @ units)
midpoint (Vector2d x1 y1) (Vector2d x2 y2) =
  Vector2d (0.5 *. (x1 .+. x2)) (0.5 *. (y1 .+. y2))

magnitude :: Vector2d (space @ units) -> Quantity units
magnitude (Vector2d vx vy) = Quantity.hypot2 vx vy

squaredMagnitude :: Units.Squared units1 units2 => Vector2d (space @ units1) -> Quantity units2
squaredMagnitude = Units.specialize . squaredMagnitude#

squaredMagnitude# :: Vector2d (space @ units) -> Quantity (units #*# units)
squaredMagnitude# (Vector2d vx vy) = vx #*# vx .+. vy #*# vy

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
angle :: Vector2d (space @ units) -> Angle
angle = (.angle)

{-| Measure the signed angle from one vector to another.

The angle will be measured counterclockwise from the first vector to the
second, and will always be between -180 and +180 degrees.
-}
angleFrom :: Vector2d (space @ units) -> Vector2d (space @ units) -> Angle
angleFrom v1 v2 = Angle.atan2 (v1 `cross#` v2) (v1 `dot#` v2)

data IsZero = IsZero deriving (Eq, Show)

instance Error.Message IsZero where
  message IsZero = "Vector is zero"

{-| Attempt to get the direction of a vector.

The current tolerance will be used to check if the vector is zero
(and therefore does not have a direction).
-}
direction :: Tolerance units => Vector2d (space @ units) -> Result IsZero (Direction2d space)
direction vector = do
  let vm = magnitude vector
  if vm ~= Quantity.zero then Failure IsZero else Success (Unit2d (vector ./. vm))

magnitudeAndDirection ::
  Tolerance units =>
  Vector2d (space @ units) ->
  Result IsZero (Quantity units, Direction2d space)
magnitudeAndDirection vector = do
  let vm = magnitude vector
  if vm ~= Quantity.zero then Failure IsZero else Success (vm, Unit2d (vector ./. vm))

{-| Normalize a vector.

If the original vector is exactly zero, then the result will be zero as well.
Otherwise, the result will be a unit vector.
-}
normalize :: Vector2d (space @ units) -> Vector2d (space @ Unitless)
normalize vector = do
  let vm = magnitude vector
  if vm == Quantity.zero then zero else vector ./. vm

-- | Rotate a vector left (counterclockwise) by 90 degrees.
rotateLeft :: Vector2d (space @ units) -> Vector2d (space @ units)
rotateLeft (Vector2d vx vy) = Vector2d (negative vy) vx

-- | Rotate a vector right (clockwise) by 90 degrees.
rotateRight :: Vector2d (space @ units) -> Vector2d (space @ units)
rotateRight (Vector2d vx vy) = Vector2d vy (negative vx)

{-# INLINE placeIn #-}
placeIn ::
  Frame2d (global @ frameUnits) (Defines local) ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
placeIn frame = placeInOrientation frame.orientation

placeInOrientation ::
  Orientation2d global ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
placeInOrientation (Orientation2d i j) (Vector2d vx vy) = vx .*. i .+. vy .*. j

{-# INLINE relativeTo #-}
relativeTo ::
  Frame2d (global @ frameUnits) (Defines local) ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation ::
  Orientation2d global ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
relativeToOrientation (Orientation2d i j) vector = Vector2d (vector `dot` i) (vector `dot` j)

{-| Convert a 2D vector to 3D vector by placing it on a plane.

Given a 2D vector defined within a plane's coordinate system,
this returns the corresponding 3D vector.
-}
{-# INLINE placeOn #-}
placeOn ::
  forall local units space planeUnits.
  Plane3d (space @ planeUnits) (Defines local) ->
  Vector2d (local @ units) ->
  Vector3d (space @ units)
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation ::
  PlaneOrientation3d global ->
  Vector2d (local @ units) ->
  Vector3d (global @ units)
placeOnOrientation (PlaneOrientation3d i j) (Vector2d vx vy) = vx .*. i .+. vy .*. j

convert :: Quantity (units2 #/# units1) -> Vector2d (space @ units1) -> Vector2d (space @ units2)
convert factor vector = Units.simplify (vector #*# factor)

unconvert :: Quantity (units2 #/# units1) -> Vector2d (space @ units2) -> Vector2d (space @ units1)
unconvert factor vector = Units.simplify (vector #/# factor)

sum :: List (Vector2d (space @ units)) -> Vector2d (space @ units)
sum = List.foldl (.+.) zero

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units)
transformBy transform vector = do
  let Transform2d _ i j = transform
  let Vector2d vx vy = vector
  vx .*. i .+. vy .*. j

{-| Rotate a vector by a given angle.

A positive angle corresponds to a counterclockwise rotation.
-}
rotateBy :: Angle -> Vector2d (space @ units) -> Vector2d (space @ units)
rotateBy theta (Vector2d vx vy) = do
  let cosTheta = Angle.cos theta
  let sinTheta = Angle.sin theta
  Vector2d (cosTheta .*. vx .-. sinTheta .*. vy) (sinTheta .*. vx .+. cosTheta .*. vy)

{-| Mirror a vector in/along a given direction.

For example, mirroring in the X direction
will negate the vector's X component and leave its Y component unchanged.
-}
mirrorIn :: Direction2d space -> Vector2d (space @ units) -> Vector2d (space @ units)
mirrorIn mirrorDirection vector = vector .-. 2.0 *. projectionIn mirrorDirection vector

{-| Mirror a vector across a given axis.

The origin point of the axis is not used, only its direction, since vectors have no position.
For example, mirroring a vector across *any* axis parallel to the X axis
will negate the vector's Y component while leaving its X component unchanged.
-}
mirrorAcross ::
  forall space units originUnits.
  Axis2d (space @ originUnits) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units)
mirrorAcross (Axis2d _ axisDirection) = mirrorIn (Direction2d.rotateLeft axisDirection)

scaleIn :: Direction2d space -> Number -> Vector2d (space @ units) -> Vector2d (space @ units)
scaleIn scaleDirection scale vector =
  vector .+. (scale .- 1.0) .*. projectionIn scaleDirection vector
