module OpenSolid.Vector2d
  ( Vector2d (Vector2d)
  , zero
  , unit
  , x
  , y
  , xy
  , fromComponents
  , from
  , meters
  , centimeters
  , millimeters
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
  , squaredMagnitude'
  , angle
  , IsZero (IsZero)
  , direction
  , magnitudeAndDirection
  , normalize
  , rotateRight
  , rotateLeft
  , placeIn
  , relativeTo
  , placeOn
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
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2d (Axis2d)
  , Basis2d (Basis2d)
  , Direction2d (Unit2d)
  , PlanarBasis3d (PlanarBasis3d)
  , Point2d
  , Transform2d (Transform2d)
  , Vector2d (Vector2d)
  , Vector3d
  )
import OpenSolid.Qty qualified as Qty
import OpenSolid.Units (Meters, SquareMeters)
import OpenSolid.Units qualified as Units

-- | The zero vector.
zero :: Vector2d (space @ units)
zero = Vector2d Qty.zero Qty.zero

-- | Construct a unit vector in the given direction.
unit :: Direction2d space -> Vector2d (space @ Unitless)
unit (Unit2d vector) = vector

{-| Construct a vector from just an X component.

The Y component will be set to zero.
-}
x :: Qty units -> Vector2d (space @ units)
x vx = Vector2d vx Qty.zero

{-| Construct a vector from just a Y component.

The X component will be set to zero.
-}
y :: Qty units -> Vector2d (space @ units)
y vy = Vector2d Qty.zero vy

-- | Construct a vector from its X and Y components.
xy :: Qty units -> Qty units -> Vector2d (space @ units)
xy = Vector2d

-- | Construct a vector from a pair of X and Y components.
fromComponents :: (Qty units, Qty units) -> Vector2d (space @ units)
fromComponents (vx, vy) = Vector2d vx vy

from :: Point2d (space @ units) -> Point2d (space @ units) -> Vector2d (space @ units)
from p1 p2 = p2 - p1

apply :: (Float -> Qty units) -> Float -> Float -> Vector2d (space @ units)
apply units px py = Vector2d (units px) (units py)

-- | Construct a vector from its X and Y components given in meters.
meters :: Float -> Float -> Vector2d (space @ Meters)
meters = apply Length.meters

-- | Construct a vector from its X and Y components given in centimeters.
centimeters :: Float -> Float -> Vector2d (space @ Meters)
centimeters = apply Length.centimeters

-- | Construct a vector from its X and Y components given in millimeters.
millimeters :: Float -> Float -> Vector2d (space @ Meters)
millimeters = apply Length.millimeters

-- | Construct a vector from its X and Y components given in inches.
inches :: Float -> Float -> Vector2d (space @ Meters)
inches = apply Length.inches

-- | Construct a vector from its X and Y components given in square meters.
squareMeters :: Float -> Float -> Vector2d (space @ SquareMeters)
squareMeters = apply Area.squareMeters

-- | Construct a vector from its magnitude (length) and angle.
polar :: Qty units -> Angle -> Vector2d (space @ units)
polar r theta = Vector2d (r * Angle.cos theta) (r * Angle.sin theta)

-- | Get the X component of a vector.
xComponent :: Vector2d (space @ units) -> Qty units
xComponent (Vector2d vx _) = vx

-- | Get the Y component of a vector.
yComponent :: Vector2d (space @ units) -> Qty units
yComponent (Vector2d _ vy) = vy

componentIn :: Direction2d space -> Vector2d (space @ units) -> Qty units
componentIn = dot

projectionIn :: Direction2d space -> Vector2d (space @ units) -> Vector2d (space @ units)
projectionIn givenDirection vector = givenDirection * componentIn givenDirection vector

-- | Get the X and Y components of a vector as a tuple.
{-# INLINE components #-}
components :: Vector2d (space @ units) -> (Qty units, Qty units)
components (Vector2d vx vy) = (vx, vy)

interpolateFrom ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Float ->
  Vector2d (space @ units)
interpolateFrom (Vector2d x1 y1) (Vector2d x2 y2) t =
  Vector2d (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))

midpoint :: Vector2d (space @ units) -> Vector2d (space @ units) -> Vector2d (space @ units)
midpoint (Vector2d x1 y1) (Vector2d x2 y2) =
  Vector2d (0.5 * (x1 + x2)) (0.5 * (y1 + y2))

magnitude :: Vector2d (space @ units) -> Qty units
magnitude (Vector2d vx vy) = Qty.hypot2 vx vy

squaredMagnitude :: Units.Squared units1 units2 => Vector2d (space @ units1) -> Qty units2
squaredMagnitude = Units.specialize . squaredMagnitude'

squaredMagnitude' :: Vector2d (space @ units) -> Qty (units :*: units)
squaredMagnitude' (Vector2d vx vy) = vx .*. vx + vy .*. vy

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
angle (Vector2d vx vy) = Angle.atan2 vy vx

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
  if vm ~= Qty.zero then Failure IsZero else Success (Unit2d (vector / vm))

magnitudeAndDirection ::
  Tolerance units =>
  Vector2d (space @ units) ->
  Result IsZero (Qty units, Direction2d space)
magnitudeAndDirection vector = do
  let vm = magnitude vector
  if vm ~= Qty.zero then Failure IsZero else Success (vm, Unit2d (vector / vm))

{-| Normalize a vector.

If the original vector is exactly zero, then the result will be zero as well.
Otherwise, the result will be a unit vector.
-}
normalize :: Vector2d (space @ units) -> Vector2d (space @ Unitless)
normalize vector = do
  let vm = magnitude vector
  if vm == Qty.zero then zero else vector / vm

rotateLeft :: Vector2d (space @ units) -> Vector2d (space @ units)
rotateLeft (Vector2d vx vy) = Vector2d (negate vy) vx

rotateRight :: Vector2d (space @ units) -> Vector2d (space @ units)
rotateRight (Vector2d vx vy) = Vector2d vy (negate vx)

placeIn ::
  Basis2d global (Defines local) ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
placeIn (Basis2d i j) (Vector2d vx vy) = vx * i + vy * j

relativeTo ::
  Basis2d global (Defines local) ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
relativeTo (Basis2d i j) vector = Vector2d (vector `dot` i) (vector `dot` j)

placeOn ::
  PlanarBasis3d space (Defines local) ->
  Vector2d (local @ units) ->
  Vector3d (space @ units)
placeOn (PlanarBasis3d i j) (Vector2d vx vy) = vx * i + vy * j

convert :: Qty (units2 :/: units1) -> Vector2d (space @ units1) -> Vector2d (space @ units2)
convert factor vector = vector !* factor

unconvert :: Qty (units2 :/: units1) -> Vector2d (space @ units2) -> Vector2d (space @ units1)
unconvert factor vector = vector !/ factor

sum :: List (Vector2d (space @ units)) -> Vector2d (space @ units)
sum = List.foldl (+) zero

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units)
transformBy transform vector = do
  let Transform2d _ i j = transform
  let Vector2d vx vy = vector
  vx * i + vy * j

rotateBy :: Angle -> Vector2d (space @ units) -> Vector2d (space @ units)
rotateBy theta (Vector2d vx vy) = do
  let cosTheta = Angle.cos theta
  let sinTheta = Angle.sin theta
  Vector2d (cosTheta * vx - sinTheta * vy) (sinTheta * vx + cosTheta * vy)

mirrorIn :: Direction2d space -> Vector2d (space @ units) -> Vector2d (space @ units)
mirrorIn mirrorDirection vector = vector - 2.0 * projectionIn mirrorDirection vector

mirrorAcross :: Axis2d (space @ originUnits) -> Vector2d (space @ units) -> Vector2d (space @ units)
mirrorAcross (Axis2d _ axisDirection) = mirrorIn (Direction2d.rotateLeft axisDirection)

scaleIn :: Direction2d space -> Float -> Vector2d (space @ units) -> Vector2d (space @ units)
scaleIn scaleDirection scale vector = vector + (scale - 1.0) * projectionIn scaleDirection vector
