module OpenSolid.Direction3d
  ( Direction3d (Direction3d)
  , xComponent
  , yComponent
  , zComponent
  , components
  , unsafe
  , x
  , y
  , z
  , positiveX
  , negativeX
  , positiveY
  , negativeY
  , positiveZ
  , negativeZ
  , perpendicularTo
  , angleFrom
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , transformBy
  , rotateIn
  , mirrorIn
  , rotateAround
  , mirrorAcross
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis3d, Basis3d, Direction3d (Unit3d), Frame3d, Plane3d)
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d

{-# COMPLETE Direction3d #-}

{-# INLINE Direction3d #-}
pattern Direction3d :: Float -> Float -> Float -> Direction3d space
pattern Direction3d dx dy dz <- Unit3d (Vector3d dx dy dz)

-- | Get the X component of a direction.
xComponent :: Direction3d space -> Float
xComponent (Direction3d dx _ _) = dx

-- | Get the Y component of a direction.
yComponent :: Direction3d space -> Float
yComponent (Direction3d _ dy _) = dy

-- | Get the Z component of a direction.
zComponent :: Direction3d space -> Float
zComponent (Direction3d _ _ dz) = dz

-- | Get the XYZ components of a direction as a tuple.
{-# INLINE components #-}
components :: Direction3d space -> (Float, Float, Float)
components (Unit3d vector) = Vector3d.components vector

unsafe :: Vector3d (space @ Unitless) -> Direction3d space
unsafe = Unit3d

{-# INLINE lift #-}
lift ::
  (Vector3d (spaceA @ Unitless) -> Vector3d (spaceB @ Unitless)) ->
  Direction3d spaceA ->
  Direction3d spaceB
lift function (Unit3d vector) = Unit3d (function vector)

-- | The positive X direction.
positiveX :: Direction3d space
positiveX = unsafe (Vector3d 1.0 0.0 0.0)

-- | The negative X direction.
negativeX :: Direction3d space
negativeX = negate positiveX

-- | The positive Y direction.
positiveY :: Direction3d space
positiveY = unsafe (Vector3d 0.0 1.0 0.0)

-- | The negative Y direction.
negativeY :: Direction3d space
negativeY = negate positiveY

-- | The positive Z direction.
positiveZ :: Direction3d space
positiveZ = unsafe (Vector3d 0.0 0.0 1.0)

-- | The negative Z direction.
negativeZ :: Direction3d space
negativeZ = negate positiveZ

-- | The X direction.
x :: Direction3d space
x = positiveX

-- | The Y direction.
y :: Direction3d space
y = positiveY

-- | The Z direction.
z :: Direction3d space
z = positiveZ

-- | Generate an arbitrary direction perpendicular to the given one.
perpendicularTo :: Direction3d space -> Direction3d space
perpendicularTo (Direction3d dx dy dz) = do
  let absX = Float.abs dx
  let absY = Float.abs dy
  let absZ = Float.abs dz
  if
    | absX <= absY && absX <= absZ -> do
        let scale = Float.hypot2 dy dz
        Unit3d (Vector3d 0.0 (-dz / scale) (dy / scale))
    | absY <= absX && absY <= absZ -> do
        let scale = Float.hypot2 dx dz
        Unit3d (Vector3d (dz / scale) 0.0 (-dx / scale))
    | otherwise -> do
        let scale = Float.hypot2 dx dy
        Unit3d (Vector3d (-dy / scale) (dx / scale) 0.0)

{-| Measure the angle from one direction to another.

The result will always be between 0 and 180 degrees.
-}
angleFrom :: Direction3d space -> Direction3d space -> Angle
angleFrom d1 d2 = Angle.atan2 (Vector3d.magnitude (d1 >< d2)) (d1 <> d2)

placeIn ::
  Frame3d (global @ originUnits) (Defines local) ->
  Direction3d local ->
  Direction3d global
placeIn frame = lift (Vector3d.placeIn frame)

relativeTo ::
  Frame3d (global @ originUnits) (Defines local) ->
  Direction3d global ->
  Direction3d local
relativeTo frame = lift (Vector3d.relativeTo frame)

placeInBasis :: Basis3d global (Defines local) -> Direction3d local -> Direction3d global
placeInBasis basis = lift (Vector3d.placeInBasis basis)

relativeToBasis :: Basis3d global (Defines local) -> Direction3d global -> Direction3d local
relativeToBasis basis = lift (Vector3d.relativeToBasis basis)

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
