module OpenSolid.Direction3d
  ( Direction3d
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
import OpenSolid.Convention3d (Convention3d)
import OpenSolid.Direction2d (Direction2d (Direction2d))
import OpenSolid.Float qualified as Float
import {-# SOURCE #-} OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d
  , Direction3d (Direction3d, Unit3d)
  , Frame3d
  , Orientation3d
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Vector3d (Vector3d)
  )
import OpenSolid.Random qualified as Random
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Vector3d qualified as Vector3d

-- | Get the XYZ components of a direction, given an XYZ coordinate convention to use.
{-# INLINE components #-}
components :: Convention3d -> Direction3d space -> (Float, Float, Float)
components convention (Unit3d vector) = Vector3d.components convention vector

unsafe :: Vector3d (space @ Unitless) -> Direction3d space
unsafe = Unit3d

{-# INLINE coerce #-}
coerce :: Direction3d space1 -> Direction3d space2
coerce (Direction3d dx dy dz) = Direction3d dx dy dz

{-# INLINE lift #-}
lift ::
  (Vector3d (spaceA @ Unitless) -> Vector3d (spaceB @ Unitless)) ->
  Direction3d spaceA ->
  Direction3d spaceB
lift function (Unit3d vector) = Unit3d (function vector)

upward :: Orientation3d space -> Direction3d space
upward = Orientation3d.upwardDirection

downward :: Orientation3d space -> Direction3d space
downward = Orientation3d.downwardDirection

forward :: Orientation3d space -> Direction3d space
forward = Orientation3d.forwardDirection

backward :: Orientation3d space -> Direction3d space
backward = Orientation3d.backwardDirection

rightward :: Orientation3d space -> Direction3d space
rightward = Orientation3d.rightwardDirection

leftward :: Orientation3d space -> Direction3d space
leftward = Orientation3d.leftwardDirection

on :: Plane3d (space @ planeUnits) (Defines local) -> Direction2d local -> Direction3d space
on (Plane3d _ (PlaneOrientation3d i j)) (Direction2d x y) = Unit3d (x * i + y * j)

polar :: Plane3d (space @ planeUnits) defines -> Angle -> Direction3d space
polar (Plane3d _ (PlaneOrientation3d i j)) angle = Unit3d (Angle.cos angle * i + Angle.sin angle * j)

-- | Generate an arbitrary direction perpendicular to the given one.
perpendicularDirection :: Direction3d space -> Direction3d space
perpendicularDirection (Direction3d dx dy dz) = do
  let absX = Float.abs dx
  let absY = Float.abs dy
  let absZ = Float.abs dz
  if
    | absX <= absY && absX <= absZ -> do
        let scale = Float.hypot2 dy dz
        Direction3d 0.0 (-dz / scale) (dy / scale)
    | absY <= absX && absY <= absZ -> do
        let scale = Float.hypot2 dx dz
        Direction3d (dz / scale) 0.0 (-dx / scale)
    | otherwise -> do
        let scale = Float.hypot2 dx dy
        Direction3d (-dy / scale) (dx / scale) 0.0

forwardComponent :: Direction3d space -> Float
forwardComponent (Unit3d vector) = Vector3d.forwardComponent vector

backwardComponent :: Direction3d space -> Float
backwardComponent (Unit3d vector) = Vector3d.backwardComponent vector

leftwardComponent :: Direction3d space -> Float
leftwardComponent (Unit3d vector) = Vector3d.leftwardComponent vector

rightwardComponent :: Direction3d space -> Float
rightwardComponent (Unit3d vector) = Vector3d.rightwardComponent vector

upwardComponent :: Direction3d space -> Float
upwardComponent (Unit3d vector) = Vector3d.upwardComponent vector

downwardComponent :: Direction3d space -> Float
downwardComponent (Unit3d vector) = Vector3d.downwardComponent vector

{-| Measure the angle from one direction to another.

The result will always be between 0 and 180 degrees.
-}
angleFrom :: Direction3d space -> Direction3d space -> Angle
angleFrom d1 d2 = Angle.atan2 (Vector3d.magnitude (d1 `cross` d2)) (d1 `dot` d2)

-- | Convert a direction defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3d (global @ frameUnits) (Defines local) -> Direction3d local -> Direction3d global
placeIn frame = lift (Vector3d.placeIn frame)

-- | Convert a direction defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Frame3d (global @ frameUnits) (Defines local) ->
  Direction3d global ->
  Direction3d local
relativeTo frame = lift (Vector3d.relativeTo frame)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag (space @ translationUnits) ->
  Direction3d space ->
  Direction3d space
transformBy transform = lift (Vector3d.transformBy transform)

{-| Rotate a direction in a given other direction.

This is equivalent to rotating around an axis with the given direction.
-}
rotateIn :: Direction3d space -> Angle -> Direction3d space -> Direction3d space
rotateIn axisDirection angle = lift (Vector3d.rotateIn axisDirection angle)

{-| Mirror a direction in a given other direction.

This is equivalent to mirroring across a plane with the given normal direction.
-}
mirrorIn :: Direction3d space -> Direction3d space -> Direction3d space
mirrorIn mirrorDirection = lift (Vector3d.mirrorIn mirrorDirection)

-- | Rotate around the given axis by the given angle.
rotateAround ::
  Axis3d (space @ axisUnits) ->
  Angle ->
  Direction3d space ->
  Direction3d space
rotateAround axis angle = lift (Vector3d.rotateAround axis angle)

-- | Mirror across the given plane.
mirrorAcross ::
  Plane3d (space @ planeUnits) defines ->
  Direction3d space ->
  Direction3d space
mirrorAcross plane = lift (Vector3d.mirrorAcross plane)

-- | Generate a random direction.
random :: Random.Generator (Direction3d space)
random = Random.do
  -- Generate a random vector to normalize
  vector <- randomVector
  let magnitude = Vector3d.magnitude vector
  -- Reject very small vectors
  -- (to avoid roundoff error during normalization),
  -- and only accept vectors inside the unit sphere
  -- (otherwise we'll get a non-uniform distribution)
  if magnitude > 0.1 && magnitude <= 1.0
    then Random.return (Unit3d (vector / magnitude))
    else random -- Generated a 'bad' vector, try again

randomVector :: Random.Generator (Vector3d (space @ Unitless))
randomVector = do
  let randomComponent = Float.random -1.0 1.0
  Random.map3 Vector3d randomComponent randomComponent randomComponent
