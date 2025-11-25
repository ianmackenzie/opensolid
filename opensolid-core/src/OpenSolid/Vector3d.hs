{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Vector3d
  ( Vector3d
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
  , IsZero (IsZero)
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
import OpenSolid.Convention3d (Convention3d (Convention3d))
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.List qualified as List
import {-# SOURCE #-} OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Direction3d (Direction3d, Unit3d)
  , Frame3d (Frame3d)
  , Orientation3d (Orientation3d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Vector2d (Vector2d)
  , Vector3d (Vector3d, Vector3d#)
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.World3d qualified as World3d

-- | The zero vector.
zero :: Vector3d space units
zero = Vector3d Quantity.zero Quantity.zero Quantity.zero

{-# INLINE coerce #-}
coerce :: Vector3d space1 units1 -> Vector3d space2 units2
coerce (Vector3d vx vy vz) = Vector3d (Quantity.coerce vx) (Quantity.coerce vy) (Quantity.coerce vz)

-- | Construct a unit vector in the given direction.
{-# INLINE unit #-}
unit :: Direction3d space -> Vector3d space Unitless
unit (Unit3d vector) = vector

-- | Construct a 3D vector on the given plane, given a 2D vector within the plane.
on ::
  Plane3d space (Defines local) ->
  Vector2d local units ->
  Vector3d space units
on (Plane3d _ (PlaneOrientation3d i j)) (Vector2d vX vY) = do
  let Direction3d iR iF iU = i
  let Direction3d jR jF jU = j
  let vR = vX .*. iR .+. vY .*. jR
  let vF = vX .*. iF .+. vY .*. jF
  let vU = vX .*. iU .+. vY .*. jU
  Vector3d vR vF vU

-- | Construct a vector from its XYZ components, given the coordinate convention to use.
xyz :: Convention3d -> (Quantity units, Quantity units, Quantity units) -> Vector3d space units
xyz Convention3d{xr, xf, xu, yr, yf, yu, zr, zf, zu} (vx, vy, vz) =
  Vector3d
    (vx .*. xr .+. vy .*. yr .+. vz .*. zr)
    (vx .*. xf .+. vy .*. yf .+. vz .*. zf)
    (vx .*. xu .+. vy .*. yu .+. vz .*. zu)

{-| Construct a vector from its XYZ components, using a Z-up convention.

This is a convention where positive X is rightward, positive Y is forward and positive Z is upward.
-}
zUp :: Quantity units -> Quantity units -> Quantity units -> Vector3d space units
zUp vX vY vZ = Vector3d vX vY vZ

{-| Construct a vector from its XYZ components, using a Y-up convention.

This is a convention where positive X is leftward, positive Y is upward, and positive Z is forward.
-}
yUp :: Quantity units -> Quantity units -> Quantity units -> Vector3d space units
yUp vX vY vZ = Vector3d (negative vX) vZ vY

componentIn :: Direction3d space -> Vector3d space units -> Quantity units
componentIn = dot

projectionIn :: Direction3d space -> Vector3d space units -> Vector3d space units
projectionIn givenDirection vector = givenDirection .*. componentIn givenDirection vector

forwardComponent :: Vector3d space units -> Quantity units
forwardComponent (Vector3d _ f _) = f

backwardComponent :: Vector3d space units -> Quantity units
backwardComponent (Vector3d _ f _) = negative f

leftwardComponent :: Vector3d space units -> Quantity units
leftwardComponent (Vector3d r _ _) = negative r

rightwardComponent :: Vector3d space units -> Quantity units
rightwardComponent (Vector3d r _ _) = r

upwardComponent :: Vector3d space units -> Quantity units
upwardComponent (Vector3d _ _ u) = u

downwardComponent :: Vector3d space units -> Quantity units
downwardComponent (Vector3d _ _ u) = negative u

-- | Get the XYZ components of a vector, given an XYZ coordinate convention to use.
components ::
  Convention3d ->
  Vector3d space units ->
  (Quantity units, Quantity units, Quantity units)
components Convention3d{xr, xf, xu, yr, yf, yu, zr, zf, zu} (Vector3d vr vf vu) =
  ( vr .*. xr .+. vf .*. xf .+. vu .*. xu
  , vr .*. yr .+. vf .*. yf .+. vu .*. yu
  , vr .*. zr .+. vf .*. zf .+. vu .*. zu
  )

{-| Get the XYZ components of a vector using a Z-up coordinate convention.

This is a convention where positive X is rightward, positive Y is forward and positive Z is upward.
-}
{-# INLINE zUpComponents #-}
zUpComponents :: Vector3d space units -> (Quantity units, Quantity units, Quantity units)
zUpComponents (Vector3d r f u) = (r, f, u)

{-# INLINE zUpComponents# #-}
zUpComponents# :: Vector3d space units -> (# Double#, Double#, Double# #)
zUpComponents# (Vector3d# r# f# u#) = (# r#, f#, u# #)

{-| Get the XYZ components of a vector using a Y-up coordinate convention.

This is a convention where positive X is leftward, positive Y is upward, and positive Z is forward.
-}
{-# INLINE yUpComponents #-}
yUpComponents :: Vector3d space units -> (Quantity units, Quantity units, Quantity units)
yUpComponents (Vector3d r f u) = (negative r, u, f)

{-# INLINE yUpComponents# #-}
yUpComponents# :: Vector3d space units -> (# Double#, Double#, Double# #)
yUpComponents# (Vector3d# r# f# u#) = (# negate# r#, u#, f# #)

interpolateFrom ::
  Vector3d space units ->
  Vector3d space units ->
  Number ->
  Vector3d space units
interpolateFrom (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) t =
  Vector3d
    (x1 .+. t .*. (x2 .-. x1))
    (y1 .+. t .*. (y2 .-. y1))
    (z1 .+. t .*. (z2 .-. z1))

midpoint :: Vector3d space units -> Vector3d space units -> Vector3d space units
midpoint (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) =
  Vector3d (0.5 *. (x1 .+. x2)) (0.5 *. (y1 .+. y2)) (0.5 *. (z1 .+. z2))

magnitude :: Vector3d space units -> Quantity units
magnitude (Vector3d vx vy vz) = Quantity.hypot3 vx vy vz

squaredMagnitude :: Units.Squared units1 units2 => Vector3d space units1 -> Quantity units2
squaredMagnitude = Units.specialize . squaredMagnitude_

squaredMagnitude_ :: Vector3d space units -> Quantity (units ?*? units)
squaredMagnitude_ (Vector3d vx vy vz) = vx ?*? vx .+. vy ?*? vy .+. vz ?*? vz

data IsZero = IsZero deriving (Eq, Show)

{-| Attempt to get the direction of a vector.

The current tolerance will be used to check if the vector is zero
(and therefore does not have a direction).
-}
direction :: Tolerance units => Vector3d space units -> Result IsZero (Direction3d space)
direction vector = do
  let vm = magnitude vector
  if vm ~= Quantity.zero then Error IsZero else Ok (Unit3d (vector ./. vm))

magnitudeAndDirection ::
  Tolerance units =>
  Vector3d space units ->
  Result IsZero (Quantity units, Direction3d space)
magnitudeAndDirection vector = do
  let vm = magnitude vector
  if vm ~= Quantity.zero then Error IsZero else Ok (vm, Unit3d (vector ./. vm))

normalize :: Vector3d space units -> Vector3d space Unitless
normalize vector = do
  let vm = magnitude vector
  if vm == Quantity.zero then zero else vector ./. vm

-- | Convert a vectr defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3d global (Defines local) -> Vector3d local units -> Vector3d global units
placeIn (Frame3d _ (Orientation3d i j k)) (Vector3d vx vy vz) = vx .*. i .+. vy .*. j .+. vz .*. k

-- | Convert a vector defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3d global (Defines local) -> Vector3d global units -> Vector3d local units
relativeTo (Frame3d _ (Orientation3d i j k)) vector =
  Vector3d (vector `dot` i) (vector `dot` j) (vector `dot` k)

projectInto :: Plane3d global (Defines local) -> Vector3d global units -> Vector2d local units
projectInto (Plane3d _ (PlaneOrientation3d i j)) v = Vector2d (v `dot` i) (v `dot` j)

sum :: List (Vector3d space units) -> Vector3d space units
sum = List.foldl (.+.) zero

convert :: Quantity (units2 ?/? units1) -> Vector3d space units1 -> Vector3d space units2
convert factor vector = Units.simplify (vector ?*? factor)

unconvert :: Quantity (units2 ?/? units1) -> Vector3d space units2 -> Vector3d space units1
unconvert factor vector = Units.simplify (vector ?/? factor)

transformBy :: Transform3d tag space -> Vector3d space units -> Vector3d space units
transformBy transform vector = do
  let Transform3d _ i j k = transform
  let Vector3d vx vy vz = vector
  vx .*. i .+. vy .*. j .+. vz .*. k

{-| Rotate a vector in a given direction.

This is equivalent to rotating around an axis with the given direction.
-}
rotateIn :: Direction3d space -> Angle -> Vector3d space units -> Vector3d space units
rotateIn axisDirection = rotateAround (Axis3d World3d.originPoint axisDirection)

{-| Scale (stretch) in the given direction by the given scaling factor.

This is equivalent to scaling along an axis with the given direction.
-}
scaleIn :: Direction3d space -> Number -> Vector3d space units -> Vector3d space units
scaleIn axisDirection = scaleAlong (Axis3d World3d.originPoint axisDirection)

{-| Mirror in a particular direction.

This is equivalent to mirroring across a plane with the given normal direction.
-}
mirrorIn :: Direction3d space -> Vector3d space units -> Vector3d space units
mirrorIn mirrorDirection vector = vector .-. 2 *. projectionIn mirrorDirection vector

-- | Rotate around the given axis by the given angle.
rotateAround :: Axis3d space -> Angle -> Vector3d space units -> Vector3d space units
rotateAround = Transform3d.rotateAroundImpl transformBy

-- | Scale (stretch) along the given axis by the given scaling factor.
scaleAlong :: Axis3d space -> Number -> Vector3d space units -> Vector3d space units
scaleAlong = Transform3d.scaleAlongImpl transformBy

-- | Mirror across the given plane.
mirrorAcross :: Plane3d space defines -> Vector3d space units -> Vector3d space units
mirrorAcross plane vector = mirrorIn (Plane3d.normalDirection plane) vector
