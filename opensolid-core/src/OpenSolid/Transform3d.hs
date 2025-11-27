module OpenSolid.Transform3d
  ( Transform3d (Transform3d)
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
import {-# SOURCE #-} OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Direction3d (Direction3d)
  , Frame3d
  , Plane3d
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point3d (Point3d, Position3d)
  , Transform3d (Transform3d)
  , Vector3d (Vector3d)
  )
import OpenSolid.Primitives qualified as Primitives
import OpenSolid.Transform qualified as Transform
import {-# SOURCE #-} OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.World3d qualified as World3d

type Rigid space = Transform3d Transform.Rigid space

type Orthonormal space = Transform3d Transform.Orthonormal space

type Uniform space = Transform3d Transform.Uniform space

type Affine space = Transform3d Transform.Affine space

unitX :: Vector3d space Unitless
unitX = Vector3d 1 0 0

unitY :: Vector3d space Unitless
unitY = Vector3d 0 1 0

unitZ :: Vector3d space Unitless
unitZ = Vector3d 0 0 1

identity :: Rigid space
identity = Transform3d World3d.originPoint unitX unitY unitZ

coerce :: Transform3d tag1 space1 -> Transform3d tag2 space2
coerce (Transform3d p0 i j k) =
  Transform3d (Point3d.coerce p0) (Vector3d.coerce i) (Vector3d.coerce j) (Vector3d.coerce k)

withFixedPoint ::
  Point3d space ->
  Vector3d space Unitless ->
  Vector3d space Unitless ->
  Vector3d space Unitless ->
  Transform3d tag space
withFixedPoint fixedPoint vx vy vz = do
  let Point3d x0 y0 z0 = fixedPoint
  let originPoint = fixedPoint .-. x0 .*. vx .-. y0 .*. vy .-. z0 .*. vz
  Transform3d originPoint vx vy vz

translateBy :: Vector3d space Meters -> Rigid space
translateBy vector = Transform3d (Position3d vector) unitX unitY unitZ

translateIn :: Direction3d space -> Length -> Rigid space
translateIn direction distance = translateBy (direction .*. distance)

translateAlong :: Axis3d space -> Length -> Rigid space
translateAlong (Axis3d _ direction) distance = translateIn direction distance

rotateAround :: Axis3d space -> Angle -> Rigid space
rotateAround axis angle = do
  let Direction3d dx dy dz = axis.direction
  let halfAngle = 0.5 *. angle
  let sinHalfAngle = Angle.sin halfAngle
  let qx = dx .*. sinHalfAngle
  let qy = dy .*. sinHalfAngle
  let qz = dz .*. sinHalfAngle
  let qw = Angle.cos halfAngle
  let wx = qw .*. qx
  let wy = qw .*. qy
  let wz = qw .*. qz
  let xx = qx .*. qx
  let xy = qx .*. qy
  let xz = qx .*. qz
  let yy = qy .*. qy
  let yz = qy .*. qz
  let zz = qz .*. qz
  let vx = Vector3d (1 -. 2 *. (yy .+. zz)) (2 *. (xy .+. wz)) (2 *. (xz .-. wy))
  let vy = Vector3d (2 *. (xy .-. wz)) (1 -. 2 *. (xx .+. zz)) (2 *. (yz .+. wx))
  let vz = Vector3d (2 *. (xz .+. wy)) (2 *. (yz .-. wx)) (1 -. 2 *. (xx .+. yy))
  withFixedPoint axis.originPoint vx vy vz

scaleAbout :: Point3d space -> Number -> Uniform space
scaleAbout point scale = do
  let vx = Vector3d scale 0 0
  let vy = Vector3d 0 scale 0
  let vz = Vector3d 0 0 scale
  withFixedPoint point vx vy vz

scaleAlong :: Axis3d space -> Number -> Affine space
scaleAlong axis scale = do
  let d = axis.direction
  let Direction3d dx dy dz = d
  -- TODO refactor to use Vector3d.scaleIn?
  let vx = unitX .+. (scale .- 1) .*. dx .*. d
  let vy = unitY .+. (scale .- 1) .*. dy .*. d
  let vz = unitZ .+. (scale .- 1) .*. dz .*. d
  withFixedPoint axis.originPoint vx vy vz

mirrorAcross :: Plane3d space defines -> Orthonormal space
mirrorAcross plane = do
  let PlaneOrientation3d i j = plane.orientation
  let Vector3d nx ny nz = i `cross` j
  let axx = 1 -. 2 *. nx .*. nx
  let ayy = 1 -. 2 *. ny .*. ny
  let azz = 1 -. 2 *. nz .*. nz
  let ayz = -2 *. ny .*. nz
  let axz = -2 *. nx .*. nz
  let axy = -2 *. nx .*. ny
  let vx = Vector3d axx axy axz
  let vy = Vector3d axy ayy ayz
  let vz = Vector3d axz ayz azz
  withFixedPoint plane.originPoint vx vy vz

placeIn :: Frame3d global local -> Transform3d tag local -> Transform3d tag global
placeIn frame transform = do
  let p0 =
        World3d.originPoint
          & Point3d.relativeTo frame
          & Point3d.transformBy transform
          & Point3d.placeIn frame
  let vx =
        unitX
          & Vector3d.relativeTo frame
          & Vector3d.transformBy transform
          & Vector3d.placeIn frame
  let vy =
        unitY
          & Vector3d.relativeTo frame
          & Vector3d.transformBy transform
          & Vector3d.placeIn frame
  let vz =
        unitZ
          & Vector3d.relativeTo frame
          & Vector3d.transformBy transform
          & Vector3d.placeIn frame
  Transform3d p0 vx vy vz

relativeTo :: Frame3d global local -> Transform3d tag global -> Transform3d tag local
relativeTo frame transform = do
  let p0 =
        World3d.originPoint
          & Point3d.placeIn frame
          & Point3d.transformBy transform
          & Point3d.relativeTo frame
  let vx =
        unitX
          & Vector3d.placeIn frame
          & Vector3d.transformBy transform
          & Vector3d.relativeTo frame
  let vy =
        unitY
          & Vector3d.placeIn frame
          & Vector3d.transformBy transform
          & Vector3d.relativeTo frame
  let vz =
        unitZ
          & Vector3d.placeIn frame
          & Vector3d.transformBy transform
          & Vector3d.relativeTo frame
  Transform3d p0 vx vy vz

toOrthonormal :: Transform.IsOrthonormal tag => Transform3d tag space -> Orthonormal space
toOrthonormal = Data.Coerce.coerce

toUniform :: Transform.IsUniform tag => Transform3d tag space -> Uniform space
toUniform = Data.Coerce.coerce

toAffine :: Transform3d tag space -> Affine space
toAffine = Data.Coerce.coerce

-- Helper functions to define specific/concrete transformation functions

translateByImpl :: (Rigid space -> a -> b) -> Vector3d space Meters -> a -> b
translateByImpl transformBy vector = transformBy (translateBy vector)

translateInImpl :: (Rigid space -> a -> b) -> Direction3d space -> Length -> a -> b
translateInImpl transformBy direction distance = transformBy (translateIn direction distance)

translateAlongImpl :: (Rigid space -> a -> b) -> Axis3d space -> Length -> a -> b
translateAlongImpl transformBy axis distance = transformBy (translateAlong axis distance)

rotateAroundImpl :: (Rigid space -> a -> b) -> Axis3d space -> Angle -> a -> b
rotateAroundImpl transformBy axis angle = transformBy (rotateAround axis angle)

mirrorAcrossImpl :: (Orthonormal space -> a -> b) -> Plane3d space defines -> a -> b
mirrorAcrossImpl transformBy plane = transformBy (mirrorAcross plane)

scaleAboutImpl :: (Uniform space -> a -> b) -> Point3d space -> Number -> a -> b
scaleAboutImpl transformBy centerPoint scale = transformBy (scaleAbout centerPoint scale)

scaleAlongImpl :: (Affine space -> a -> b) -> Axis3d space -> Number -> a -> b
scaleAlongImpl transformBy axis scale = transformBy (scaleAlong axis scale)
