module OpenSolid.Transform3d
  ( Transform3d (Transform3d)
  , Rigid
  , Orthonormal
  , Uniform
  , Affine
  , identity
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
  , translateByOwnImpl
  , translateInOwnImpl
  , translateAlongOwnImpl
  , rotateAroundOwnImpl
  , mirrorAcrossOwnImpl
  , scaleAboutOwnImpl
  , scaleAlongOwnImpl
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import {-# SOURCE #-} OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude hiding (identity)
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Direction3d (Unit3d)
  , Frame3d
  , PlanarBasis3d (PlanarBasis3d)
  , Plane3d (Plane3d)
  , Point3d (Point3d)
  , Transform3d (Transform3d)
  , Vector3d (Vector3d)
  )
import OpenSolid.Transform qualified as Transform
import {-# SOURCE #-} OpenSolid.Vector3d qualified as Vector3d

type Rigid coordinateSystem = Transform3d Transform.Rigid coordinateSystem

type Orthonormal coordinateSystem = Transform3d Transform.Orthonormal coordinateSystem

type Uniform coordinateSystem = Transform3d Transform.Uniform coordinateSystem

type Affine coordinateSystem = Transform3d Transform.Affine coordinateSystem

unitX :: Vector3d (space @ Unitless)
unitX = Vector3d 1.0 0.0 0.0

unitY :: Vector3d (space @ Unitless)
unitY = Vector3d 0.0 1.0 0.0

unitZ :: Vector3d (space @ Unitless)
unitZ = Vector3d 0.0 0.0 1.0

identity :: Rigid (space @ units)
identity = Transform3d Point3d.origin unitX unitY unitZ

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
translateBy vector = Transform3d (Point3d.origin + vector) unitX unitY unitZ

translateIn :: Direction3d space -> Qty units -> Rigid (space @ units)
translateIn direction distance = translateBy (direction * distance)

translateAlong :: Axis3d (space @ units) -> Qty units -> Rigid (space @ units)
translateAlong (Axis3d _ direction) distance = translateIn direction distance

rotateAround :: Axis3d (space @ units) -> Angle -> Rigid (space @ units)
rotateAround (Axis3d originPoint direction) angle = do
  let Unit3d (Vector3d dx dy dz) = direction
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
  withFixedPoint originPoint vx vy vz

scaleAbout :: Point3d (space @ units) -> Float -> Uniform (space @ units)
scaleAbout point scale = do
  let vx = Vector3d scale 0.0 0.0
  let vy = Vector3d 0.0 scale 0.0
  let vz = Vector3d 0.0 0.0 scale
  withFixedPoint point vx vy vz

scaleAlong :: Axis3d (space @ units) -> Float -> Affine (space @ units)
scaleAlong (Axis3d originPoint direction) scale = do
  let Unit3d (Vector3d dx dy dz) = direction
  -- TODO refactor to use Vector3d.scaleIn?
  let vx = unitX + (scale - 1.0) * dx * direction
  let vy = unitY + (scale - 1.0) * dy * direction
  let vz = unitZ + (scale - 1.0) * dz * direction
  withFixedPoint originPoint vx vy vz

mirrorAcross :: Plane3d (space @ units) defines -> Orthonormal (space @ units)
mirrorAcross (Plane3d p0 (PlanarBasis3d i j)) = do
  let Vector3d nx ny nz = i >< j
  let axx = 1.0 - 2.0 * nx * nx
  let ayy = 1.0 - 2.0 * ny * ny
  let azz = 1.0 - 2.0 * nz * nz
  let ayz = -2.0 * ny * nz
  let axz = -2.0 * nx * nz
  let axy = -2.0 * nx * ny
  let vx = Vector3d axx axy axz
  let vy = Vector3d axy ayy ayz
  let vz = Vector3d axz ayz azz
  withFixedPoint p0 vx vy vz

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Transform3d tag (local @ units) ->
  Transform3d tag (global @ units)
placeIn frame transform = do
  let p0 = Point3d.origin |> Point3d.relativeTo frame |> Point3d.transformBy transform |> Point3d.placeIn frame
  let vx = unitX |> Vector3d.relativeTo frame |> Vector3d.transformBy transform |> Vector3d.placeIn frame
  let vy = unitY |> Vector3d.relativeTo frame |> Vector3d.transformBy transform |> Vector3d.placeIn frame
  let vz = unitZ |> Vector3d.relativeTo frame |> Vector3d.transformBy transform |> Vector3d.placeIn frame
  Transform3d p0 vx vy vz

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Transform3d tag (global @ units) ->
  Transform3d tag (local @ units)
relativeTo frame transform = do
  let p0 = Point3d.origin |> Point3d.placeIn frame |> Point3d.transformBy transform |> Point3d.relativeTo frame
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

translateByOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Vector3d (space @ units)) -> a -> b
translateByOwnImpl transformBy getVector argument =
  transformBy (translateBy (getVector argument)) argument

translateInOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Direction3d space) -> Qty units -> a -> b
translateInOwnImpl transformBy getDirection distance argument =
  transformBy (translateIn (getDirection argument) distance) argument

translateAlongOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Axis3d (space @ units)) -> Qty units -> a -> b
translateAlongOwnImpl transformBy getAxis distance argument =
  transformBy (translateAlong (getAxis argument) distance) argument

rotateAroundOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Axis3d (space @ units)) -> Angle -> a -> b
rotateAroundOwnImpl transformBy getAxis angle argument =
  transformBy (rotateAround (getAxis argument) angle) argument

mirrorAcrossOwnImpl :: (Orthonormal (space @ units) -> a -> b) -> (a -> Plane3d (space @ units) defines) -> a -> b
mirrorAcrossOwnImpl transformBy getPlane argument =
  transformBy (mirrorAcross (getPlane argument)) argument

scaleAboutOwnImpl :: (Uniform (space @ units) -> a -> b) -> (a -> Point3d (space @ units)) -> Float -> a -> b
scaleAboutOwnImpl transformBy getCenterPoint scale argument =
  transformBy (scaleAbout (getCenterPoint argument) scale) argument

scaleAlongOwnImpl :: (Affine (space @ units) -> a -> b) -> (a -> Axis3d (space @ units)) -> Float -> a -> b
scaleAlongOwnImpl transformBy getAxis scale argument =
  transformBy (scaleAlong (getAxis argument) scale) argument
