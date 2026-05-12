module OpenSolid.VectorTransform3D
  ( VectorTransform3D
  , Rigid
  , Orthonormal
  , Uniform
  , Affine
  , identity
  , coerce
  , scaleBy
  , scaleIn
  , scaleAlong
  , rotateIn
  , rotateAround
  , mirrorIn
  , mirrorAcross
  , placeIn
  , relativeTo
  , asOrthonormal
  , asUniform
  , asAffine
  , scaleByImpl
  , scaleInImpl
  , scaleAlongImpl
  , rotateInImpl
  , rotateAroundImpl
  , mirrorInImpl
  , mirrorAcrossImpl
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D (..)
  , Direction3D (Direction3D, Unit3D)
  , Frame3D
  , Plane3D (Plane3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  , Vector3D (Vector3D)
  , VectorTransform3D (VectorTransform3D)
  )
import {-# SOURCE #-} OpenSolid.Transform qualified as Transform

type Rigid space = VectorTransform3D Transform.Rigid space

type Orthonormal space = VectorTransform3D Transform.Orthonormal space

type Uniform space = VectorTransform3D Transform.Uniform space

type Affine space = VectorTransform3D Transform.Affine space

unitX :: Vector3D Unitless space
unitX = Vector3D 1.0 0.0 0.0

unitY :: Vector3D Unitless space
unitY = Vector3D 0.0 1.0 0.0

unitZ :: Vector3D Unitless space
unitZ = Vector3D 0.0 0.0 1.0

identity :: Rigid space
identity = VectorTransform3D unitX unitY unitZ

coerce :: VectorTransform3D tag1 space1 -> VectorTransform3D tag2 space2
coerce = Data.Coerce.coerce

scaleBy :: Number -> Uniform space
scaleBy scale = do
  let vx = Vector3D scale 0.0 0.0
  let vy = Vector3D 0.0 scale 0.0
  let vz = Vector3D 0.0 0.0 scale
  VectorTransform3D vx vy vz

scaleIn :: Direction3D space -> Number -> Affine space
scaleIn direction scale = do
  let Direction3D dx dy dz = direction
  let vx = unitX + (scale - 1.0) * dx * direction
  let vy = unitY + (scale - 1.0) * dy * direction
  let vz = unitZ + (scale - 1.0) * dz * direction
  VectorTransform3D vx vy vz

scaleAlong :: Axis3D space -> Number -> Affine space
scaleAlong axis = scaleIn axis.direction

rotateIn :: Direction3D space -> Angle -> Rigid space
rotateIn (Direction3D dx dy dz) angle = do
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
  VectorTransform3D vx vy vz

rotateAround :: Axis3D space -> Angle -> Rigid space
rotateAround axis = rotateIn axis.direction

mirrorIn :: Direction3D space -> Orthonormal space
mirrorIn (Direction3D dx dy dz) = do
  let axx = 1.0 - 2.0 * dx * dx
  let ayy = 1.0 - 2.0 * dy * dy
  let azz = 1.0 - 2.0 * dz * dz
  let ayz = -2.0 * dy * dz
  let axz = -2.0 * dx * dz
  let axy = -2.0 * dx * dy
  let vx = Vector3D axx axy axz
  let vy = Vector3D axy ayy ayz
  let vz = Vector3D axz ayz azz
  VectorTransform3D vx vy vz

mirrorAcross :: Plane3D space -> Orthonormal space
mirrorAcross (Plane3D _ (PlaneOrientation3D i j)) = mirrorIn (Unit3D (i `cross` j))

placeIn :: Frame3D global local -> VectorTransform3D tag local -> VectorTransform3D tag global
placeIn frame transform = do
  let vx = unitX / frame * transform * frame
  let vy = unitY / frame * transform * frame
  let vz = unitZ / frame * transform * frame
  VectorTransform3D vx vy vz

relativeTo :: Frame3D global local -> VectorTransform3D tag global -> VectorTransform3D tag local
relativeTo frame transform = do
  let vx = unitX * frame * transform / frame
  let vy = unitY * frame * transform / frame
  let vz = unitZ * frame * transform / frame
  VectorTransform3D vx vy vz

asOrthonormal :: Transform.IsOrthonormal tag => VectorTransform3D tag space -> Orthonormal space
asOrthonormal = Data.Coerce.coerce

asUniform :: Transform.IsUniform tag => VectorTransform3D tag space -> Uniform space
asUniform = Data.Coerce.coerce

asAffine :: VectorTransform3D tag space -> Affine space
asAffine = Data.Coerce.coerce

-- Helper functions to define specific/concrete transformation functions

{-# INLINE scaleByImpl #-}
scaleByImpl :: (Uniform space -> a -> b) -> Number -> a -> b
scaleByImpl transformBy scale = transformBy (scaleBy scale)

{-# INLINE scaleInImpl #-}
scaleInImpl :: (Affine space -> a -> b) -> Direction3D space -> Number -> a -> b
scaleInImpl transformBy direction scale = transformBy (scaleIn direction scale)

{-# INLINE scaleAlongImpl #-}
scaleAlongImpl :: (Affine space -> a -> b) -> Axis3D space -> Number -> a -> b
scaleAlongImpl transformBy axis scale = transformBy (scaleAlong axis scale)

{-# INLINE rotateInImpl #-}
rotateInImpl :: (Rigid space -> a -> b) -> Direction3D space -> Angle -> a -> b
rotateInImpl transformBy direction angle = transformBy (rotateIn direction angle)

{-# INLINE rotateAroundImpl #-}
rotateAroundImpl :: (Rigid space -> a -> b) -> Axis3D space -> Angle -> a -> b
rotateAroundImpl transformBy axis angle = transformBy (rotateAround axis angle)

{-# INLINE mirrorInImpl #-}
mirrorInImpl :: (Orthonormal space -> a -> b) -> Direction3D space -> a -> b
mirrorInImpl transformBy direction = transformBy (mirrorIn direction)

{-# INLINE mirrorAcrossImpl #-}
mirrorAcrossImpl :: (Orthonormal space -> a -> b) -> Plane3D space -> a -> b
mirrorAcrossImpl transformBy plane = transformBy (mirrorAcross plane)
