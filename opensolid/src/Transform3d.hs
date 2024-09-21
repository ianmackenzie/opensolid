module Transform3d
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
  -- , mirrorAcross
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
  -- , mirrorAcrossImpl
  , scaleAboutImpl
  , scaleAlongImpl
  , translateByOwnImpl
  , translateInOwnImpl
  , translateAlongOwnImpl
  , rotateAroundOwnImpl
  -- , mirrorAcrossOwnImpl
  , scaleAboutOwnImpl
  , scaleAlongOwnImpl
  )
where

import Angle qualified
import {-# SOURCE #-} Axis3d (Axis3d)
import {-# SOURCE #-} Axis3d qualified
import Data.Coerce qualified
import {-# SOURCE #-} Direction3d (Direction3d)
import {-# SOURCE #-} Direction3d qualified
import {-# SOURCE #-} Frame3d (Frame3d)
import OpenSolid hiding (identity)
import {-# SOURCE #-} Point3d (Point3d)
import {-# SOURCE #-} Point3d qualified
import Point3d.CoordinateTransformation qualified as Point3d
import Transform qualified
import Units qualified
import {-# SOURCE #-} Vector3d (Vector3d)
import {-# SOURCE #-} Vector3d qualified
import Vector3d.CoordinateTransformation qualified as Vector3d

type role Transform3d phantom nominal

type Transform3d :: Type -> CoordinateSystem -> Type
data Transform3d tag (coordinateSystem :: CoordinateSystem) where
  Transform3d_ ::
    Point3d (space @ units) ->
    Vector3d (space @ Unitless) ->
    Vector3d (space @ Unitless) ->
    Vector3d (space @ Unitless) ->
    Transform3d tag (space @ units)

deriving instance Eq (Transform3d tag (space @ units))

deriving instance Show (Transform3d tag (space @ units))

{-# COMPLETE Transform3d #-}

{-# INLINE Transform3d #-}
pattern Transform3d ::
  Point3d (space @ units) ->
  Vector3d (space @ Unitless) ->
  Vector3d (space @ Unitless) ->
  Vector3d (space @ Unitless) ->
  Transform3d tag (space @ units)
pattern Transform3d p0 vx vy vz <- Transform3d_ p0 vx vy vz

instance HasUnits (Transform3d tag (space @ units)) where
  type UnitsOf (Transform3d tag (space @ units)) = units

instance
  (tag1 ~ tag2, space1 ~ space2) =>
  Units.Coercion
    (Transform3d tag1 (space1 @ unitsA))
    (Transform3d tag2 (space2 @ unitsB))
  where
  coerce (Transform3d p0 vx vy vz) = Transform3d_ (Units.coerce p0) vx vy vz

type Rigid coordinateSystem = Transform3d Transform.Rigid coordinateSystem

type Orthonormal coordinateSystem = Transform3d Transform.Orthonormal coordinateSystem

type Uniform coordinateSystem = Transform3d Transform.Uniform coordinateSystem

type Affine coordinateSystem = Transform3d Transform.Affine coordinateSystem

instance
  ( Composition tag1 tag2 tag3
  , space ~ space_
  , units ~ units_
  ) =>
  Composition
    (Transform3d tag1 (space @ units))
    (Transform3d tag2 (space_ @ units_))
    (Transform3d tag3 (space @ units))
  where
  transform1 >> transform2 =
    Transform3d_
      (Point3d.origin |> Point3d.transformBy transform1 |> Point3d.transformBy transform2)
      (unitX |> Vector3d.transformBy transform1 |> Vector3d.transformBy transform2)
      (unitY |> Vector3d.transformBy transform1 |> Vector3d.transformBy transform2)
      (unitZ |> Vector3d.transformBy transform1 |> Vector3d.transformBy transform2)

unitX :: Vector3d (space @ Unitless)
unitX = Vector3d.xyz 1.0 0.0 0.0

unitY :: Vector3d (space @ Unitless)
unitY = Vector3d.xyz 0.0 1.0 0.0

unitZ :: Vector3d (space @ Unitless)
unitZ = Vector3d.xyz 0.0 0.0 1.0

identity :: Rigid (space @ units)
identity = Transform3d_ Point3d.origin unitX unitY unitZ

withFixedPoint ::
  Point3d (space @ units) ->
  Vector3d (space @ Unitless) ->
  Vector3d (space @ Unitless) ->
  Vector3d (space @ Unitless) ->
  Transform3d tag (space @ units)
withFixedPoint fixedPoint vx vy vz = do
  let (x0, y0, z0) = Point3d.coordinates fixedPoint
  Transform3d_ (fixedPoint - x0 * vx - y0 * vy - z0 * vz) vx vy vz

translateBy :: Vector3d (space @ units) -> Rigid (space @ units)
translateBy vector = Transform3d_ (Point3d.origin + vector) unitX unitY unitZ

translateIn :: Direction3d space -> Qty units -> Rigid (space @ units)
translateIn direction distance = translateBy (direction * distance)

translateAlong :: Axis3d (space @ units) -> Qty units -> Rigid (space @ units)
translateAlong axis distance = translateIn (Axis3d.direction axis) distance

rotateAround :: Axis3d (space @ units) -> Angle -> Rigid (space @ units)
rotateAround axis angle = do
  let (dx, dy, dz) = Direction3d.components (Axis3d.direction axis)
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
  let vx = Vector3d.xyz (1 - 2 * (yy + zz)) (2 * (xy + wz)) (2 * (xz - wy))
  let vy = Vector3d.xyz (2 * (xy - wz)) (1 - 2 * (xx + zz)) (2 * (yz + wx))
  let vz = Vector3d.xyz (2 * (xz + wy)) (2 * (yz - wx)) (1 - 2 * (xx + yy))
  withFixedPoint (Axis3d.originPoint axis) vx vy vz

scaleAbout :: Point3d (space @ units) -> Float -> Uniform (space @ units)
scaleAbout point scale = do
  let vx = Vector3d.xyz scale 0.0 0.0
  let vy = Vector3d.xyz 0.0 scale 0.0
  let vz = Vector3d.xyz 0.0 0.0 scale
  withFixedPoint point vx vy vz

scaleAlong :: Axis3d (space @ units) -> Float -> Affine (space @ units)
scaleAlong axis scale = do
  let d = Axis3d.direction axis
  let (dx, dy, dz) = Direction3d.components d
  -- TODO refactor to use Vector3d.scaleIn?
  let vx = unitX + (scale - 1) * dx * d
  let vy = unitY + (scale - 1) * dy * d
  let vz = unitZ + (scale - 1) * dz * d
  withFixedPoint (Axis3d.originPoint axis) vx vy vz

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Transform3d tag (local @ units) ->
  Transform3d tag (global @ units)
placeIn frame transform = do
  let p0 = Point3d.origin |> Point3d.relativeTo frame |> Point3d.transformBy transform |> Point3d.placeIn frame
  let vx = unitX |> Vector3d.relativeTo frame |> Vector3d.transformBy transform |> Vector3d.placeIn frame
  let vy = unitY |> Vector3d.relativeTo frame |> Vector3d.transformBy transform |> Vector3d.placeIn frame
  let vz = unitZ |> Vector3d.relativeTo frame |> Vector3d.transformBy transform |> Vector3d.placeIn frame
  Transform3d_ p0 vx vy vz

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Transform3d tag (global @ units) ->
  Transform3d tag (local @ units)
relativeTo frame transform = do
  let p0 = Point3d.origin |> Point3d.placeIn frame |> Point3d.transformBy transform |> Point3d.relativeTo frame
  let vx = unitX |> Vector3d.placeIn frame |> Vector3d.transformBy transform |> Vector3d.relativeTo frame
  let vy = unitY |> Vector3d.placeIn frame |> Vector3d.transformBy transform |> Vector3d.relativeTo frame
  let vz = unitZ |> Vector3d.placeIn frame |> Vector3d.transformBy transform |> Vector3d.relativeTo frame
  Transform3d_ p0 vx vy vz

toOrthonormal :: Transform.IsOrthonormal tag => Transform3d tag (space @ units) -> Orthonormal (space @ units)
toOrthonormal = Data.Coerce.coerce

toUniform :: Transform.IsUniform tag => Transform3d tag (space @ units) -> Uniform (space @ units)
toUniform = Data.Coerce.coerce

toAffine :: Transform3d tag (space @ units) -> Affine (space @ units)
toAffine = Data.Coerce.coerce

-- Helper functions to define specific/concrete transformation functions

translateByImpl :: (Rigid (space @ units) -> a -> b) -> Vector3d (space @ units) -> a -> b
translateByImpl transformBy vector = transformBy (translateBy vector)

translateByOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Vector3d (space @ units)) -> a -> b
translateByOwnImpl transformBy getVector argument =
  transformBy (translateBy (getVector argument)) argument

translateInImpl :: (Rigid (space @ units) -> a -> b) -> Direction3d space -> Qty units -> a -> b
translateInImpl transformBy direction distance = transformBy (translateIn direction distance)

translateInOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Direction3d space) -> Qty units -> a -> b
translateInOwnImpl transformBy getDirection distance argument =
  transformBy (translateIn (getDirection argument) distance) argument

translateAlongImpl :: (Rigid (space @ units) -> a -> b) -> Axis3d (space @ units) -> Qty units -> a -> b
translateAlongImpl transformBy axis distance = transformBy (translateAlong axis distance)

translateAlongOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Axis3d (space @ units)) -> Qty units -> a -> b
translateAlongOwnImpl transformBy getAxis distance argument =
  transformBy (translateAlong (getAxis argument) distance) argument

rotateAroundImpl :: (Rigid (space @ units) -> a -> b) -> Axis3d (space @ units) -> Angle -> a -> b
rotateAroundImpl transformBy axis angle = transformBy (rotateAround axis angle)

rotateAroundOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Axis3d (space @ units)) -> Angle -> a -> b
rotateAroundOwnImpl transformBy getAxis angle argument =
  transformBy (rotateAround (getAxis argument) angle) argument

-- mirrorAcrossImpl :: (Orthonormal (space @ units) -> a -> b) -> Axis3d (space @ units) -> a -> b
-- mirrorAcrossImpl transformBy axis = transformBy (mirrorAcross axis)

-- mirrorAcrossOwnImpl :: (Orthonormal (space @ units) -> a -> b) -> (a -> Axis3d (space @ units)) -> a -> b
-- mirrorAcrossOwnImpl transformBy getAxis argument =
--   transformBy (mirrorAcross (getAxis argument)) argument

scaleAboutImpl :: (Uniform (space @ units) -> a -> b) -> Point3d (space @ units) -> Float -> a -> b
scaleAboutImpl transformBy centerPoint scale = transformBy (scaleAbout centerPoint scale)

scaleAboutOwnImpl :: (Uniform (space @ units) -> a -> b) -> (a -> Point3d (space @ units)) -> Float -> a -> b
scaleAboutOwnImpl transformBy getCenterPoint scale argument =
  transformBy (scaleAbout (getCenterPoint argument) scale) argument

scaleAlongImpl :: (Affine (space @ units) -> a -> b) -> Axis3d (space @ units) -> Float -> a -> b
scaleAlongImpl transformBy axis scale = transformBy (scaleAlong axis scale)

scaleAlongOwnImpl :: (Affine (space @ units) -> a -> b) -> (a -> Axis3d (space @ units)) -> Float -> a -> b
scaleAlongOwnImpl transformBy getAxis scale argument =
  transformBy (scaleAlong (getAxis argument) scale) argument
