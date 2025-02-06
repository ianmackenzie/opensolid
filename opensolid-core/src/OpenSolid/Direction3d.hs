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
  , angleFrom
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , transformBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Prelude
import OpenSolid.Primitives (Basis3d, Direction3d (Unit3d), Frame3d)
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d

{-# COMPLETE Direction3d #-}

{-# INLINE Direction3d #-}
pattern Direction3d :: Float -> Float -> Float -> Direction3d space
pattern Direction3d dx dy dz <- Unit3d (Vector3d dx dy dz)

xComponent :: Direction3d space -> Float
xComponent (Direction3d dx _ _) = dx

yComponent :: Direction3d space -> Float
yComponent (Direction3d _ dy _) = dy

zComponent :: Direction3d space -> Float
zComponent (Direction3d _ _ dz) = dz

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

positiveX :: Direction3d space
positiveX = unsafe (Vector3d 1.0 0.0 0.0)

negativeX :: Direction3d space
negativeX = negate positiveX

positiveY :: Direction3d space
positiveY = unsafe (Vector3d 0.0 1.0 0.0)

negativeY :: Direction3d space
negativeY = negate positiveY

positiveZ :: Direction3d space
positiveZ = unsafe (Vector3d 0.0 0.0 1.0)

negativeZ :: Direction3d space
negativeZ = negate positiveZ

x :: Direction3d space
x = positiveX

y :: Direction3d space
y = positiveY

z :: Direction3d space
z = positiveZ

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
