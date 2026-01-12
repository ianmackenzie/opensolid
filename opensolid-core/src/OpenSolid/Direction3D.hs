module OpenSolid.Direction3D
  ( Direction3D
  , components
  , unsafe
  , coerce
  , upward
  , downward
  , forward
  , backward
  , rightward
  , leftward
  , on
  , polar
  , perpendicularDirection
  , forwardComponent
  , backwardComponent
  , rightwardComponent
  , leftwardComponent
  , upwardComponent
  , downwardComponent
  , angleFrom
  , placeIn
  , relativeTo
  , transformBy
  , rotateIn
  , mirrorIn
  , rotateAround
  , mirrorAcross
  , random
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Convention3D (Convention3D)
import OpenSolid.Direction2D (Direction2D (Direction2D))
import OpenSolid.Number qualified as Number
import {-# SOURCE #-} OpenSolid.Orientation3D qualified as Orientation3D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D
  , Direction3D (Direction3D, Unit3D)
  , Frame3D
  , Orientation3D
  , Plane3D (Plane3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  , Vector3D (Vector3D)
  )
import OpenSolid.Random qualified as Random
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.Vector3D qualified as Vector3D

-- | Get the XYZ components of a direction, given an XYZ coordinate convention to use.
{-# INLINE components #-}
components :: Convention3D -> Direction3D space -> (Number, Number, Number)
components convention (Unit3D vector) = Vector3D.components convention vector

unsafe :: Vector3D Unitless space -> Direction3D space
unsafe = Unit3D

{-# INLINE coerce #-}
coerce :: Direction3D space1 -> Direction3D space2
coerce (Direction3D dx dy dz) = Direction3D dx dy dz

{-# INLINE lift #-}
lift ::
  (Vector3D Unitless spaceA -> Vector3D Unitless spaceB) ->
  Direction3D spaceA ->
  Direction3D spaceB
lift function (Unit3D vector) = Unit3D (function vector)

upward :: Orientation3D space -> Direction3D space
upward = Orientation3D.upwardDirection

downward :: Orientation3D space -> Direction3D space
downward = Orientation3D.downwardDirection

forward :: Orientation3D space -> Direction3D space
forward = Orientation3D.forwardDirection

backward :: Orientation3D space -> Direction3D space
backward = Orientation3D.backwardDirection

rightward :: Orientation3D space -> Direction3D space
rightward = Orientation3D.rightwardDirection

leftward :: Orientation3D space -> Direction3D space
leftward = Orientation3D.leftwardDirection

on :: Plane3D global local -> Direction2D local -> Direction3D global
on (Plane3D _ (PlaneOrientation3D i j)) (Direction2D x y) = Unit3D (x .*. i .+. y .*. j)

polar :: Plane3D global local -> Angle -> Direction3D global
polar (Plane3D _ (PlaneOrientation3D i j)) angle =
  Unit3D (Angle.cos angle .*. i .+. Angle.sin angle .*. j)

-- | Generate an arbitrary direction perpendicular to the given one.
perpendicularDirection :: Direction3D space -> Direction3D space
perpendicularDirection (Direction3D dx dy dz) = do
  let absX = Number.abs dx
  let absY = Number.abs dy
  let absZ = Number.abs dz
  if
    | absX <= absY && absX <= absZ -> do
        let scale = Number.hypot2 dy dz
        Direction3D 0 (-dz ./. scale) (dy ./. scale)
    | absY <= absX && absY <= absZ -> do
        let scale = Number.hypot2 dx dz
        Direction3D (dz ./. scale) 0 (-dx ./. scale)
    | otherwise -> do
        let scale = Number.hypot2 dx dy
        Direction3D (-dy ./. scale) (dx ./. scale) 0

forwardComponent :: Direction3D space -> Number
forwardComponent (Unit3D vector) = Vector3D.forwardComponent vector

backwardComponent :: Direction3D space -> Number
backwardComponent (Unit3D vector) = Vector3D.backwardComponent vector

leftwardComponent :: Direction3D space -> Number
leftwardComponent (Unit3D vector) = Vector3D.leftwardComponent vector

rightwardComponent :: Direction3D space -> Number
rightwardComponent (Unit3D vector) = Vector3D.rightwardComponent vector

upwardComponent :: Direction3D space -> Number
upwardComponent (Unit3D vector) = Vector3D.upwardComponent vector

downwardComponent :: Direction3D space -> Number
downwardComponent (Unit3D vector) = Vector3D.downwardComponent vector

{-| Measure the angle from one direction to another.

The result will always be between 0 and 180 degrees.
-}
angleFrom :: Direction3D space -> Direction3D space -> Angle
angleFrom d1 d2 = Angle.atan2 (Vector3D.magnitude (d1 `cross` d2)) (d1 `dot` d2)

-- | Convert a direction defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Direction3D local -> Direction3D global
placeIn frame = lift (Vector3D.placeIn frame)

-- | Convert a direction defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Direction3D global -> Direction3D local
relativeTo frame = lift (Vector3D.relativeTo frame)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3D tag space ->
  Direction3D space ->
  Direction3D space
transformBy transform = lift (Vector3D.transformBy transform)

{-| Rotate a direction in a given other direction.

This is equivalent to rotating around an axis with the given direction.
-}
rotateIn :: Direction3D space -> Angle -> Direction3D space -> Direction3D space
rotateIn axisDirection angle = lift (Vector3D.rotateIn axisDirection angle)

{-| Mirror a direction in a given other direction.

This is equivalent to mirroring across a plane with the given normal direction.
-}
mirrorIn :: Direction3D space -> Direction3D space -> Direction3D space
mirrorIn mirrorDirection = lift (Vector3D.mirrorIn mirrorDirection)

-- | Rotate around the given axis by the given angle.
rotateAround :: Axis3D space -> Angle -> Direction3D space -> Direction3D space
rotateAround axis angle = lift (Vector3D.rotateAround axis angle)

-- | Mirror across the given plane.
mirrorAcross :: Plane3D global local -> Direction3D global -> Direction3D global
mirrorAcross plane = lift (Vector3D.mirrorAcross plane)

-- | Generate a random direction.
random :: Random.Generator (Direction3D space)
random = do
  -- Generate a random vector to normalize
  vector <- randomVector
  let magnitude = Vector3D.magnitude vector
  -- Reject very small vectors
  -- (to avoid roundoff error during normalization),
  -- and only accept vectors inside the unit sphere
  -- (otherwise we'll get a non-uniform distribution)
  if magnitude > 0.1 && magnitude <= 1
    then Random.return (Unit3D (vector ./. magnitude))
    else random -- Generated a 'bad' vector, try again

randomVector :: Random.Generator (Vector3D Unitless space)
randomVector = do
  let randomComponent = Number.random -1 1
  Random.map3 Vector3D randomComponent randomComponent randomComponent
