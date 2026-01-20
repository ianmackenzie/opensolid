{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Vector3D
  ( Vector3D
  , zero
  , coerce
  , unit
  , on
  , xyz
  , zUp
  , yUp
  , componentIn
  , projectionIn
  , forwardComponent
  , backwardComponent
  , rightwardComponent
  , leftwardComponent
  , upwardComponent
  , downwardComponent
  , components
  , zUpComponents
  , zUpComponents#
  , yUpComponents
  , yUpComponents#
  , midpoint
  , interpolateFrom
  , magnitude
  , squaredMagnitude
  , squaredMagnitude_
  , direction
  , magnitudeAndDirection
  , normalize
  , placeIn
  , relativeTo
  , projectInto
  , sum
  , convert
  , unconvert
  , transformBy
  , rotateIn
  , rotateAround
  , scaleIn
  , scaleAlong
  , mirrorIn
  , mirrorAcross
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Convention3D (Convention3D (Convention3D))
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.List qualified as List
import {-# SOURCE #-} OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D (Axis3D)
  , Direction3D (Direction3D, Unit3D)
  , Frame3D (Frame3D)
  , Orientation3D (Orientation3D)
  , Plane3D (Plane3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  , Vector2D (Vector2D)
  , Vector3D (Vector3D, Vector3D#)
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform3D (Transform3D (Transform3D))
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.Vector qualified as Vector
import OpenSolid.World3D qualified as World3D

-- | The zero vector.
zero :: Vector3D units space
zero = Vector3D Quantity.zero Quantity.zero Quantity.zero

{-# INLINE coerce #-}
coerce :: Vector3D units1 space1 -> Vector3D units2 space2
coerce (Vector3D vx vy vz) = Vector3D (Quantity.coerce vx) (Quantity.coerce vy) (Quantity.coerce vz)

-- | Construct a unit vector in the given direction.
{-# INLINE unit #-}
unit :: Direction3D space -> Vector3D Unitless space
unit (Unit3D vector) = vector

-- | Construct a 3D vector on the given plane, given a 2D vector within the plane.
on :: Plane3D global local -> Vector2D units local -> Vector3D units global
on (Plane3D _ (PlaneOrientation3D i j)) (Vector2D vX vY) = do
  let Direction3D iR iF iU = i
  let Direction3D jR jF jU = j
  let vR = vX .*. iR .+. vY .*. jR
  let vF = vX .*. iF .+. vY .*. jF
  let vU = vX .*. iU .+. vY .*. jU
  Vector3D vR vF vU

-- | Construct a vector from its XYZ components, given the coordinate convention to use.
xyz :: Convention3D -> (Quantity units, Quantity units, Quantity units) -> Vector3D units space
xyz Convention3D{xr, xf, xu, yr, yf, yu, zr, zf, zu} (vx, vy, vz) =
  Vector3D
    (vx .*. xr .+. vy .*. yr .+. vz .*. zr)
    (vx .*. xf .+. vy .*. yf .+. vz .*. zf)
    (vx .*. xu .+. vy .*. yu .+. vz .*. zu)

{-| Construct a vector from its XYZ components, using a Z-up convention.

This is a convention where positive X is rightward, positive Y is forward and positive Z is upward.
-}
zUp :: Quantity units -> Quantity units -> Quantity units -> Vector3D units space
zUp vX vY vZ = Vector3D vX vY vZ

{-| Construct a vector from its XYZ components, using a Y-up convention.

This is a convention where positive X is leftward, positive Y is upward, and positive Z is forward.
-}
yUp :: Quantity units -> Quantity units -> Quantity units -> Vector3D units space
yUp vX vY vZ = Vector3D (negative vX) vZ vY

componentIn :: Direction3D space -> Vector3D units space -> Quantity units
componentIn = dot

projectionIn :: Direction3D space -> Vector3D units space -> Vector3D units space
projectionIn givenDirection vector = givenDirection .*. componentIn givenDirection vector

forwardComponent :: Vector3D units space -> Quantity units
forwardComponent (Vector3D _ f _) = f

backwardComponent :: Vector3D units space -> Quantity units
backwardComponent (Vector3D _ f _) = negative f

leftwardComponent :: Vector3D units space -> Quantity units
leftwardComponent (Vector3D r _ _) = negative r

rightwardComponent :: Vector3D units space -> Quantity units
rightwardComponent (Vector3D r _ _) = r

upwardComponent :: Vector3D units space -> Quantity units
upwardComponent (Vector3D _ _ u) = u

downwardComponent :: Vector3D units space -> Quantity units
downwardComponent (Vector3D _ _ u) = negative u

-- | Get the XYZ components of a vector, given an XYZ coordinate convention to use.
components ::
  Convention3D ->
  Vector3D units space ->
  (Quantity units, Quantity units, Quantity units)
components Convention3D{xr, xf, xu, yr, yf, yu, zr, zf, zu} (Vector3D vr vf vu) =
  ( vr .*. xr .+. vf .*. xf .+. vu .*. xu
  , vr .*. yr .+. vf .*. yf .+. vu .*. yu
  , vr .*. zr .+. vf .*. zf .+. vu .*. zu
  )

{-| Get the XYZ components of a vector using a Z-up coordinate convention.

This is a convention where positive X is rightward, positive Y is forward and positive Z is upward.
-}
{-# INLINE zUpComponents #-}
zUpComponents :: Vector3D units space -> (Quantity units, Quantity units, Quantity units)
zUpComponents (Vector3D r f u) = (r, f, u)

{-# INLINE zUpComponents# #-}
zUpComponents# :: Vector3D units space -> (# Double#, Double#, Double# #)
zUpComponents# (Vector3D# r# f# u#) = (# r#, f#, u# #)

{-| Get the XYZ components of a vector using a Y-up coordinate convention.

This is a convention where positive X is leftward, positive Y is upward, and positive Z is forward.
-}
{-# INLINE yUpComponents #-}
yUpComponents :: Vector3D units space -> (Quantity units, Quantity units, Quantity units)
yUpComponents (Vector3D r f u) = (negative r, u, f)

{-# INLINE yUpComponents# #-}
yUpComponents# :: Vector3D units space -> (# Double#, Double#, Double# #)
yUpComponents# (Vector3D# r# f# u#) = (# negate# r#, u#, f# #)

interpolateFrom ::
  Vector3D units space ->
  Vector3D units space ->
  Number ->
  Vector3D units space
interpolateFrom (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) t =
  Vector3D
    (x1 .+. t .*. (x2 .-. x1))
    (y1 .+. t .*. (y2 .-. y1))
    (z1 .+. t .*. (z2 .-. z1))

midpoint :: Vector3D units space -> Vector3D units space -> Vector3D units space
midpoint (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) =
  Vector3D (0.5 *. (x1 .+. x2)) (0.5 *. (y1 .+. y2)) (0.5 *. (z1 .+. z2))

magnitude :: Vector3D units space -> Quantity units
magnitude (Vector3D vx vy vz) = Quantity.hypot3 vx vy vz

squaredMagnitude :: Units.Squared units1 units2 => Vector3D units1 space -> Quantity units2
squaredMagnitude = Units.specialize . squaredMagnitude_

squaredMagnitude_ :: Vector3D units space -> Quantity (units ?*? units)
squaredMagnitude_ (Vector3D vx vy vz) = vx ?*? vx .+. vy ?*? vy .+. vz ?*? vz

{-| Attempt to get the direction of a vector.

The current tolerance will be used to check if the vector is zero
(and therefore does not have a direction).
-}
direction :: Tolerance units => Vector3D units space -> Result Vector.IsZero (Direction3D space)
direction = Vector.direction

magnitudeAndDirection ::
  Tolerance units =>
  Vector3D units space ->
  Result Vector.IsZero (Quantity units, Direction3D space)
magnitudeAndDirection = Vector.magnitudeAndDirection

normalize :: Vector3D units space -> Vector3D Unitless space
normalize vector = do
  let vm = magnitude vector
  if vm == Quantity.zero then zero else vector ./. vm

-- | Convert a vectr defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Vector3D units local -> Vector3D units global
placeIn (Frame3D _ (Orientation3D i j k)) (Vector3D vx vy vz) = vx .*. i .+. vy .*. j .+. vz .*. k

-- | Convert a vector defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Vector3D units global -> Vector3D units local
relativeTo (Frame3D _ (Orientation3D i j k)) vector =
  Vector3D (vector `dot` i) (vector `dot` j) (vector `dot` k)

projectInto :: Plane3D global local -> Vector3D units global -> Vector2D units local
projectInto (Plane3D _ (PlaneOrientation3D i j)) v = Vector2D (v `dot` i) (v `dot` j)

sum :: List (Vector3D units space) -> Vector3D units space
sum = List.foldl (.+.) zero

convert :: Quantity (units2 ?/? units1) -> Vector3D units1 space -> Vector3D units2 space
convert factor vector = Units.simplify (vector ?*? factor)

unconvert :: Quantity (units2 ?/? units1) -> Vector3D units2 space -> Vector3D units1 space
unconvert factor vector = Units.simplify (vector ?/? factor)

transformBy :: Transform3D tag space -> Vector3D units space -> Vector3D units space
transformBy transform vector = do
  let Transform3D _ i j k = transform
  let Vector3D vx vy vz = vector
  vx .*. i .+. vy .*. j .+. vz .*. k

{-| Rotate a vector in a given direction.

This is equivalent to rotating around an axis with the given direction.
-}
rotateIn :: Direction3D space -> Angle -> Vector3D units space -> Vector3D units space
rotateIn axisDirection = rotateAround (Axis3D World3D.originPoint axisDirection)

{-| Scale (stretch) in the given direction by the given scaling factor.

This is equivalent to scaling along an axis with the given direction.
-}
scaleIn :: Direction3D space -> Number -> Vector3D units space -> Vector3D units space
scaleIn axisDirection = scaleAlong (Axis3D World3D.originPoint axisDirection)

{-| Mirror in a particular direction.

This is equivalent to mirroring across a plane with the given normal direction.
-}
mirrorIn :: Direction3D space -> Vector3D units space -> Vector3D units space
mirrorIn mirrorDirection vector = vector .-. 2 *. projectionIn mirrorDirection vector

-- | Rotate around the given axis by the given angle.
rotateAround :: Axis3D space -> Angle -> Vector3D units space -> Vector3D units space
rotateAround = Transform3D.rotateAroundImpl transformBy

-- | Scale (stretch) along the given axis by the given scaling factor.
scaleAlong :: Axis3D space -> Number -> Vector3D units space -> Vector3D units space
scaleAlong = Transform3D.scaleAlongImpl transformBy

-- | Mirror across the given plane.
mirrorAcross :: Plane3D global local -> Vector3D units global -> Vector3D units global
mirrorAcross plane vector = mirrorIn (Plane3D.normalDirection plane) vector
