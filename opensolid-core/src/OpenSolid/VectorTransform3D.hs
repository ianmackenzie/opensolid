module OpenSolid.VectorTransform3D
  ( VectorTransform3D (VectorTransform3D)
  , Rigid
  , Orthonormal
  , Uniform
  , Affine
  , identity
  , coerce
  , rotateAround
  , scaleBy
  , scaleIn
  , mirrorAcross
  , placeIn
  , relativeTo
  , toOrthonormal
  , toUniform
  , toAffine
  , rotateAroundImpl
  , mirrorAcrossImpl
  , scaleByImpl
  , scaleInImpl
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction3D (Direction3D)
  , Frame3D
  , PlaneOrientation3D (PlaneOrientation3D)
  , Vector3D (Vector3D)
  , VectorTransform3D (VectorTransform3D)
  )
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector3D qualified as Vector3D

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

rotateAround :: Direction3D space -> Angle -> Rigid space
rotateAround (Direction3D dx dy dz) angle = do
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

scaleBy :: Number -> Uniform space
scaleBy scale = do
  let vx = Vector3D scale 0.0 0.0
  let vy = Vector3D 0.0 scale 0.0
  let vz = Vector3D 0.0 0.0 scale
  VectorTransform3D vx vy vz

scaleIn :: Direction3D space -> Number -> Affine space
scaleIn direction scale = do
  let Direction3D dx dy dz = direction
  -- TODO refactor to use Vector3D.scaleIn?
  let vx = unitX + (scale - 1.0) * dx * direction
  let vy = unitY + (scale - 1.0) * dy * direction
  let vz = unitZ + (scale - 1.0) * dz * direction
  VectorTransform3D vx vy vz

mirrorAcross :: PlaneOrientation3D space -> Orthonormal space
mirrorAcross (PlaneOrientation3D i j) = do
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
  VectorTransform3D vx vy vz

placeIn :: Frame3D global local -> VectorTransform3D tag local -> VectorTransform3D tag global
placeIn frame transform = do
  let vx =
        unitX
          & Vector3D.relativeTo frame
          & (* transform)
          & Vector3D.placeIn frame
  let vy =
        unitY
          & Vector3D.relativeTo frame
          & (* transform)
          & Vector3D.placeIn frame
  let vz =
        unitZ
          & Vector3D.relativeTo frame
          & (* transform)
          & Vector3D.placeIn frame
  VectorTransform3D vx vy vz

relativeTo :: Frame3D global local -> VectorTransform3D tag global -> VectorTransform3D tag local
relativeTo frame transform = do
  let vx =
        unitX
          & Vector3D.placeIn frame
          & (* transform)
          & Vector3D.relativeTo frame
  let vy =
        unitY
          & Vector3D.placeIn frame
          & (* transform)
          & Vector3D.relativeTo frame
  let vz =
        unitZ
          & Vector3D.placeIn frame
          & (* transform)
          & Vector3D.relativeTo frame
  VectorTransform3D vx vy vz

toOrthonormal :: Transform.IsOrthonormal tag => VectorTransform3D tag space -> Orthonormal space
toOrthonormal = Data.Coerce.coerce

toUniform :: Transform.IsUniform tag => VectorTransform3D tag space -> Uniform space
toUniform = Data.Coerce.coerce

toAffine :: VectorTransform3D tag space -> Affine space
toAffine = Data.Coerce.coerce

-- Helper functions to define specific/concrete transformation functions

rotateAroundImpl :: (Rigid space -> a -> b) -> Direction3D space -> Angle -> a -> b
rotateAroundImpl transformBy direction angle = transformBy (rotateAround direction angle)

mirrorAcrossImpl :: (Orthonormal space -> a -> b) -> PlaneOrientation3D space -> a -> b
mirrorAcrossImpl transformBy planeOrientation = transformBy (mirrorAcross planeOrientation)

scaleByImpl :: (Uniform space -> a -> b) -> Number -> a -> b
scaleByImpl transformBy scale = transformBy (scaleBy scale)

scaleInImpl :: (Affine space -> a -> b) -> Direction3D space -> Number -> a -> b
scaleInImpl transformBy direction scale = transformBy (scaleIn direction scale)
