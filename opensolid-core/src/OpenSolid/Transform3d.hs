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
import {-# SOURCE #-} OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude hiding (identity)
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
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform qualified as Transform
import {-# SOURCE #-} OpenSolid.Vector3d qualified as Vector3d

type Rigid coordinateSystem = Transform3d Transform.Rigid coordinateSystem

type Orthonormal coordinateSystem = Transform3d Transform.Orthonormal coordinateSystem

type Uniform coordinateSystem = Transform3d Transform.Uniform coordinateSystem

type Affine coordinateSystem = Transform3d Transform.Affine coordinateSystem

originPoint :: Point3d (space @ units)
originPoint = Point3d Qty.zero Qty.zero Qty.zero

unitX :: Vector3d (space @ Unitless)
unitX = Vector3d 1.0 0.0 0.0

unitY :: Vector3d (space @ Unitless)
unitY = Vector3d 0.0 1.0 0.0

unitZ :: Vector3d (space @ Unitless)
unitZ = Vector3d 0.0 0.0 1.0

identity :: Rigid (space @ units)
identity = Transform3d originPoint unitX unitY unitZ

coerce :: Transform3d tag1 (space1 @ units1) -> Transform3d tag2 (space2 @ units2)
coerce (Transform3d p0 i j k) =
  Transform3d (Point3d.coerce p0) (Vector3d.coerce i) (Vector3d.coerce j) (Vector3d.coerce k)

withFixedPoint ::
  Point3d (space @ units) ->
  Vector3d (space @ Unitless) ->
  Vector3d (space @ Unitless) ->
  Vector3d (space @ Unitless) ->
  Transform3d tag (space @ units)
withFixedPoint fixedPoint vx vy vz = do
  let Point3d x0 y0 z0 = fixedPoint
  Transform3d (fixedPoint - x0 * vx - y0 * vy - z0 * vz) vx vy vz

translateBy :: Vector3d (space @ units) -> Rigid (space @ units)
translateBy vector = Transform3d (Position3d vector) unitX unitY unitZ

translateIn :: Direction3d space -> Qty units -> Rigid (space @ units)
translateIn direction distance = translateBy (direction * distance)

translateAlong :: Axis3d (space @ units) -> Qty units -> Rigid (space @ units)
translateAlong (Axis3d _ direction) distance = translateIn direction distance

rotateAround :: Axis3d (space @ units) -> Angle -> Rigid (space @ units)
rotateAround axis angle = do
  let Direction3d dx dy dz = axis.direction
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
  let vx = Vector3d (1.0 - 2.0 * (yy + zz)) (2.0 * (xy + wz)) (2.0 * (xz - wy))
  let vy = Vector3d (2.0 * (xy - wz)) (1.0 - 2.0 * (xx + zz)) (2.0 * (yz + wx))
  let vz = Vector3d (2.0 * (xz + wy)) (2.0 * (yz - wx)) (1.0 - 2.0 * (xx + yy))
  withFixedPoint axis.originPoint vx vy vz

scaleAbout :: Point3d (space @ units) -> Float -> Uniform (space @ units)
scaleAbout point scale = do
  let vx = Vector3d scale 0.0 0.0
  let vy = Vector3d 0.0 scale 0.0
  let vz = Vector3d 0.0 0.0 scale
  withFixedPoint point vx vy vz

scaleAlong :: Axis3d (space @ units) -> Float -> Affine (space @ units)
scaleAlong axis scale = do
  let d = axis.direction
  let Direction3d dx dy dz = d
  -- TODO refactor to use Vector3d.scaleIn?
  let vx = unitX + (scale - 1.0) * dx * d
  let vy = unitY + (scale - 1.0) * dy * d
  let vz = unitZ + (scale - 1.0) * dz * d
  withFixedPoint axis.originPoint vx vy vz

mirrorAcross :: Plane3d (space @ units) defines -> Orthonormal (space @ units)
mirrorAcross plane = do
  let PlaneOrientation3d i j = plane.orientation
  let Vector3d nx ny nz = i `cross` j
  let axx = 1.0 - 2.0 * nx * nx
  let ayy = 1.0 - 2.0 * ny * ny
  let azz = 1.0 - 2.0 * nz * nz
  let ayz = -2.0 * ny * nz
  let axz = -2.0 * nx * nz
  let axy = -2.0 * nx * ny
  let vx = Vector3d axx axy axz
  let vy = Vector3d axy ayy ayz
  let vz = Vector3d axz ayz azz
  withFixedPoint plane.originPoint vx vy vz

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Transform3d tag (local @ units) ->
  Transform3d tag (global @ units)
placeIn frame transform = do
  let p0 = originPoint |> Point3d.relativeTo frame |> Point3d.transformBy transform |> Point3d.placeIn frame
  let vx = unitX |> Vector3d.relativeTo frame |> Vector3d.transformBy transform |> Vector3d.placeIn frame
  let vy = unitY |> Vector3d.relativeTo frame |> Vector3d.transformBy transform |> Vector3d.placeIn frame
  let vz = unitZ |> Vector3d.relativeTo frame |> Vector3d.transformBy transform |> Vector3d.placeIn frame
  Transform3d p0 vx vy vz

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Transform3d tag (global @ units) ->
  Transform3d tag (local @ units)
relativeTo frame transform = do
  let p0 = originPoint |> Point3d.placeIn frame |> Point3d.transformBy transform |> Point3d.relativeTo frame
  let vx = unitX |> Vector3d.placeIn frame |> Vector3d.transformBy transform |> Vector3d.relativeTo frame
  let vy = unitY |> Vector3d.placeIn frame |> Vector3d.transformBy transform |> Vector3d.relativeTo frame
  let vz = unitZ |> Vector3d.placeIn frame |> Vector3d.transformBy transform |> Vector3d.relativeTo frame
  Transform3d p0 vx vy vz

toOrthonormal :: Transform.IsOrthonormal tag => Transform3d tag (space @ units) -> Orthonormal (space @ units)
toOrthonormal = Data.Coerce.coerce

toUniform :: Transform.IsUniform tag => Transform3d tag (space @ units) -> Uniform (space @ units)
toUniform = Data.Coerce.coerce

toAffine :: Transform3d tag (space @ units) -> Affine (space @ units)
toAffine = Data.Coerce.coerce

-- Helper functions to define specific/concrete transformation functions

translateByImpl :: (Rigid (space @ units) -> a -> b) -> Vector3d (space @ units) -> a -> b
translateByImpl transformBy vector = transformBy (translateBy vector)

translateInImpl :: (Rigid (space @ units) -> a -> b) -> Direction3d space -> Qty units -> a -> b
translateInImpl transformBy direction distance = transformBy (translateIn direction distance)

translateAlongImpl :: (Rigid (space @ units) -> a -> b) -> Axis3d (space @ units) -> Qty units -> a -> b
translateAlongImpl transformBy axis distance = transformBy (translateAlong axis distance)

rotateAroundImpl :: (Rigid (space @ units) -> a -> b) -> Axis3d (space @ units) -> Angle -> a -> b
rotateAroundImpl transformBy axis angle = transformBy (rotateAround axis angle)

mirrorAcrossImpl :: (Orthonormal (space @ units) -> a -> b) -> Plane3d (space @ units) defines -> a -> b
mirrorAcrossImpl transformBy plane = transformBy (mirrorAcross plane)

scaleAboutImpl :: (Uniform (space @ units) -> a -> b) -> Point3d (space @ units) -> Float -> a -> b
scaleAboutImpl transformBy centerPoint scale = transformBy (scaleAbout centerPoint scale)

scaleAlongImpl :: (Affine (space @ units) -> a -> b) -> Axis3d (space @ units) -> Float -> a -> b
scaleAlongImpl transformBy axis scale = transformBy (scaleAlong axis scale)
