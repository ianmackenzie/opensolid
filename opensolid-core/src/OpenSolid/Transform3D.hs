module OpenSolid.Transform3D
  ( Transform3D (Transform3D)
  , Rigid
  , Orthonormal
  , Uniform
  , Affine
  , identity
  , coerce
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , scaleAbout
  , scaleAlong
  , placeIn
  , relativeTo
  , toOrthonormal
  , toUniform
  , toAffine
  , translateByImpl
  , translateInImpl
  , translateAlongImpl
  , rotateAroundImpl
  , mirrorAcrossImpl
  , scaleAboutImpl
  , scaleAlongImpl
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Length (Length)
import {-# SOURCE #-} OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D (Axis3D)
  , Direction3D (Direction3D)
  , Frame3D
  , Plane3D
  , PlaneOrientation3D (PlaneOrientation3D)
  , Point3D (Point3D, Position3D)
  , Transform3D (Transform3D)
  , Vector3D (Vector3D)
  )
import OpenSolid.Primitives qualified as Primitives
import OpenSolid.Transform qualified as Transform
import {-# SOURCE #-} OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.World3D qualified as World3D

type Rigid space = Transform3D Transform.Rigid space

type Orthonormal space = Transform3D Transform.Orthonormal space

type Uniform space = Transform3D Transform.Uniform space

type Affine space = Transform3D Transform.Affine space

unitX :: Vector3D Unitless space
unitX = Vector3D 1.0 0.0 0.0

unitY :: Vector3D Unitless space
unitY = Vector3D 0.0 1.0 0.0

unitZ :: Vector3D Unitless space
unitZ = Vector3D 0.0 0.0 1.0

identity :: Rigid space
identity = Transform3D World3D.originPoint unitX unitY unitZ

coerce :: Transform3D tag1 space1 -> Transform3D tag2 space2
coerce (Transform3D p0 i j k) =
  Transform3D (Point3D.coerce p0) (Vector3D.coerce i) (Vector3D.coerce j) (Vector3D.coerce k)

withFixedPoint ::
  Point3D space ->
  Vector3D Unitless space ->
  Vector3D Unitless space ->
  Vector3D Unitless space ->
  Transform3D tag space
withFixedPoint fixedPoint vx vy vz = do
  let Point3D x0 y0 z0 = fixedPoint
  let originPoint = fixedPoint - x0 * vx - y0 * vy - z0 * vz
  Transform3D originPoint vx vy vz

translateBy :: Vector3D Meters space -> Rigid space
translateBy vector = Transform3D (Position3D vector) unitX unitY unitZ

translateIn :: Direction3D space -> Length -> Rigid space
translateIn direction distance = translateBy (direction * distance)

translateAlong :: Axis3D space -> Length -> Rigid space
translateAlong (Axis3D _ direction) distance = translateIn direction distance

rotateAround :: Axis3D space -> Angle -> Rigid space
rotateAround axis angle = do
  let Direction3D dx dy dz = axis.direction
  let halfAngle = 0.5 * angle
  let sinHalfAngle = Angle.sin halfAngle
  let qx = dx * sinHalfAngle
  let qy = dy * sinHalfAngle
  let qz = dz * sinHalfAngle
  let qw = Angle.cos halfAngle
  let wx = qw * qx
  let wy = qw * qy
  let wz = qw * qz
  let xx = qx * qx
  let xy = qx * qy
  let xz = qx * qz
  let yy = qy * qy
  let yz = qy * qz
  let zz = qz * qz
  let vx = Vector3D (1.0 - 2.0 * (yy + zz)) (2.0 * (xy + wz)) (2.0 * (xz - wy))
  let vy = Vector3D (2.0 * (xy - wz)) (1.0 - 2.0 * (xx + zz)) (2.0 * (yz + wx))
  let vz = Vector3D (2.0 * (xz + wy)) (2.0 * (yz - wx)) (1.0 - 2.0 * (xx + yy))
  withFixedPoint axis.originPoint vx vy vz

scaleAbout :: Point3D space -> Number -> Uniform space
scaleAbout point scale = do
  let vx = Vector3D scale 0.0 0.0
  let vy = Vector3D 0.0 scale 0.0
  let vz = Vector3D 0.0 0.0 scale
  withFixedPoint point vx vy vz

scaleAlong :: Axis3D space -> Number -> Affine space
scaleAlong axis scale = do
  let d = axis.direction
  let Direction3D dx dy dz = d
  -- TODO refactor to use Vector3D.scaleIn?
  let vx = unitX + (scale - 1.0) * dx * d
  let vy = unitY + (scale - 1.0) * dy * d
  let vz = unitZ + (scale - 1.0) * dz * d
  withFixedPoint axis.originPoint vx vy vz

mirrorAcross :: Plane3D global local -> Orthonormal global
mirrorAcross plane = do
  let PlaneOrientation3D i j = plane.orientation
  let Vector3D nx ny nz = i `cross` j
  let axx = 1.0 - 2.0 * nx * nx
  let ayy = 1.0 - 2.0 * ny * ny
  let azz = 1.0 - 2.0 * nz * nz
  let ayz = -2.0 * ny * nz
  let axz = -2.0 * nx * nz
  let axy = -2.0 * nx * ny
  let vx = Vector3D axx axy axz
  let vy = Vector3D axy ayy ayz
  let vz = Vector3D axz ayz azz
  withFixedPoint plane.originPoint vx vy vz

placeIn :: Frame3D global local -> Transform3D tag local -> Transform3D tag global
placeIn frame transform = do
  let p0 =
        World3D.originPoint
          & Point3D.relativeTo frame
          & Point3D.transformBy transform
          & Point3D.placeIn frame
  let vx =
        unitX
          & Vector3D.relativeTo frame
          & Vector3D.transformBy transform
          & Vector3D.placeIn frame
  let vy =
        unitY
          & Vector3D.relativeTo frame
          & Vector3D.transformBy transform
          & Vector3D.placeIn frame
  let vz =
        unitZ
          & Vector3D.relativeTo frame
          & Vector3D.transformBy transform
          & Vector3D.placeIn frame
  Transform3D p0 vx vy vz

relativeTo :: Frame3D global local -> Transform3D tag global -> Transform3D tag local
relativeTo frame transform = do
  let p0 =
        World3D.originPoint
          & Point3D.placeIn frame
          & Point3D.transformBy transform
          & Point3D.relativeTo frame
  let vx =
        unitX
          & Vector3D.placeIn frame
          & Vector3D.transformBy transform
          & Vector3D.relativeTo frame
  let vy =
        unitY
          & Vector3D.placeIn frame
          & Vector3D.transformBy transform
          & Vector3D.relativeTo frame
  let vz =
        unitZ
          & Vector3D.placeIn frame
          & Vector3D.transformBy transform
          & Vector3D.relativeTo frame
  Transform3D p0 vx vy vz

toOrthonormal :: Transform.IsOrthonormal tag => Transform3D tag space -> Orthonormal space
toOrthonormal = Data.Coerce.coerce

toUniform :: Transform.IsUniform tag => Transform3D tag space -> Uniform space
toUniform = Data.Coerce.coerce

toAffine :: Transform3D tag space -> Affine space
toAffine = Data.Coerce.coerce

-- Helper functions to define specific/concrete transformation functions

translateByImpl :: (Rigid space -> a -> b) -> Vector3D Meters space -> a -> b
translateByImpl transformBy vector = transformBy (translateBy vector)

translateInImpl :: (Rigid space -> a -> b) -> Direction3D space -> Length -> a -> b
translateInImpl transformBy direction distance = transformBy (translateIn direction distance)

translateAlongImpl :: (Rigid space -> a -> b) -> Axis3D space -> Length -> a -> b
translateAlongImpl transformBy axis distance = transformBy (translateAlong axis distance)

rotateAroundImpl :: (Rigid space -> a -> b) -> Axis3D space -> Angle -> a -> b
rotateAroundImpl transformBy axis angle = transformBy (rotateAround axis angle)

mirrorAcrossImpl :: (Orthonormal global -> a -> b) -> Plane3D global local -> a -> b
mirrorAcrossImpl transformBy plane = transformBy (mirrorAcross plane)

scaleAboutImpl :: (Uniform space -> a -> b) -> Point3D space -> Number -> a -> b
scaleAboutImpl transformBy centerPoint scale = transformBy (scaleAbout centerPoint scale)

scaleAlongImpl :: (Affine space -> a -> b) -> Axis3D space -> Number -> a -> b
scaleAlongImpl transformBy axis scale = transformBy (scaleAlong axis scale)
