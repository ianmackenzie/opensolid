module Transform2d
  ( Transform2d (Transform2d)
  , Rigid
  , Uniform
  , Affine
  , IsRigid
  , IsUniform
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
  , fromRigid
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

import Angle qualified
import {-# SOURCE #-} Axis2d (Axis2d)
import {-# SOURCE #-} Axis2d qualified
import Data.Coerce qualified
import {-# SOURCE #-} Direction2d (Direction2d)
import {-# SOURCE #-} Direction2d qualified
import {-# SOURCE #-} Frame2d (Frame2d)
import OpenSolid hiding (identity)
import {-# SOURCE #-} Point2d (Point2d)
import {-# SOURCE #-} Point2d qualified
import Point2d.CoordinateTransformation qualified as Point2d
import Units qualified
import {-# SOURCE #-} Vector2d (Vector2d)
import {-# SOURCE #-} Vector2d qualified
import Vector2d.CoordinateTransformation qualified as Vector2d

type role Transform2d phantom nominal

type Transform2d :: Type -> CoordinateSystem -> Type
data Transform2d a (coordinateSystem :: CoordinateSystem) where
  Transform2d_ ::
    Point2d (space @ units) ->
    Vector2d (space @ Unitless) ->
    Vector2d (space @ Unitless) ->
    Transform2d a (space @ units)

deriving instance Show (Transform2d a (space @ units))

{-# COMPLETE Transform2d #-}

{-# INLINE Transform2d #-}
pattern Transform2d ::
  Point2d (space @ units) ->
  Vector2d (space @ Unitless) ->
  Vector2d (space @ Unitless) ->
  Transform2d a (space @ units)
pattern Transform2d p0 vx vy <- Transform2d_ p0 vx vy

instance HasUnits (Transform2d a (space @ units)) where
  type Units (Transform2d a (space @ units)) = units
  type Erase (Transform2d a (space @ units)) = Transform2d a (space @ Unitless)

instance
  (a ~ a_, space ~ space_) =>
  Units.Coercion
    (Transform2d a (space @ units1))
    (Transform2d a_ (space_ @ units2))
  where
  coerce (Transform2d p0 vx vy) = Transform2d_ (Units.coerce p0) vx vy

data RigidTag = RigidTag

data UniformTag = UniformTag

data AffineTag = AffineTag

type Rigid coordinateSystem = Transform2d RigidTag coordinateSystem

type Uniform coordinateSystem = Transform2d UniformTag coordinateSystem

type Affine coordinateSystem = Transform2d AffineTag coordinateSystem

class IsRigid a

class IsUniform a

instance IsRigid RigidTag

instance IsUniform RigidTag

instance IsUniform UniformTag

instance Composition RigidTag RigidTag RigidTag where RigidTag >> RigidTag = RigidTag

instance Composition RigidTag UniformTag UniformTag where RigidTag >> UniformTag = UniformTag

instance Composition RigidTag AffineTag AffineTag where RigidTag >> AffineTag = AffineTag

instance Composition UniformTag RigidTag UniformTag where UniformTag >> RigidTag = UniformTag

instance Composition UniformTag UniformTag UniformTag where UniformTag >> UniformTag = UniformTag

instance Composition UniformTag AffineTag AffineTag where UniformTag >> AffineTag = AffineTag

instance Composition AffineTag RigidTag AffineTag where AffineTag >> RigidTag = AffineTag

instance Composition AffineTag UniformTag AffineTag where AffineTag >> UniformTag = AffineTag

instance Composition AffineTag AffineTag AffineTag where AffineTag >> AffineTag = AffineTag

instance
  ( Composition a b c
  , space ~ space_
  , units ~ units_
  ) =>
  Composition
    (Transform2d a (space @ units))
    (Transform2d b (space_ @ units_))
    (Transform2d c (space @ units))
  where
  transform1 >> transform2 =
    Transform2d_
      (Point2d.origin |> Point2d.transformBy transform1 |> Point2d.transformBy transform2)
      (unitX |> Vector2d.transformBy transform1 |> Vector2d.transformBy transform2)
      (unitY |> Vector2d.transformBy transform1 |> Vector2d.transformBy transform2)

unitX :: Vector2d (space @ Unitless)
unitX = Vector2d.xy 1.0 0.0

unitY :: Vector2d (space @ Unitless)
unitY = Vector2d.xy 0.0 1.0

identity :: Rigid (space @ units)
identity = Transform2d_ Point2d.origin unitX unitY

withFixedPoint :: Point2d (space @ units) -> Vector2d (space @ Unitless) -> Vector2d (space @ Unitless) -> Transform2d a (space @ units)
withFixedPoint fixedPoint vx vy = do
  let (fixedX, fixedY) = Point2d.coordinates fixedPoint
  Transform2d_ (fixedPoint - fixedX * vx - fixedY * vy) vx vy

translateBy :: Vector2d (space @ units) -> Rigid (space @ units)
translateBy vector = Transform2d_ (Point2d.origin + vector) unitX unitY

translateIn :: Direction2d space -> Qty units -> Rigid (space @ units)
translateIn direction distance = translateBy (direction * distance)

translateAlong :: Axis2d (space @ units) -> Qty units -> Rigid (space @ units)
translateAlong axis distance = translateIn (Axis2d.direction axis) distance

rotateAround :: Point2d (space @ units) -> Angle -> Rigid (space @ units)
rotateAround centerPoint angle = do
  let cos = Angle.cos angle
  let sin = Angle.sin angle
  let vx = Vector2d.xy cos sin
  let vy = Vector2d.xy -sin cos
  withFixedPoint centerPoint vx vy

mirrorAcross :: Axis2d (space @ units) -> Rigid (space @ units)
mirrorAcross axis = do
  let (dx, dy) = Direction2d.components (Axis2d.direction axis)
  let vx = Vector2d.xy (1 - 2 * dy * dy) (2 * dx * dy)
  let vy = Vector2d.xy (2 * dx * dy) (1 - 2 * dx * dx)
  withFixedPoint (Axis2d.originPoint axis) vx vy

scaleAbout :: Point2d (space @ units) -> Float -> Uniform (space @ units)
scaleAbout point scale = do
  let vx = Vector2d.xy scale 0.0
  let vy = Vector2d.xy 0.0 scale
  withFixedPoint point vx vy

scaleAlong :: Axis2d (space @ units) -> Float -> Affine (space @ units)
scaleAlong axis scale = do
  let (dx, dy) = Direction2d.components (Axis2d.direction axis)
  let dx2 = dx * dx
  let dy2 = dy * dy
  let xy = (scale - 1) * dx * dy
  let vx = Vector2d.xy (scale * dx2 + dy2) xy
  let vy = Vector2d.xy xy (scale * dy2 + dx2)
  withFixedPoint (Axis2d.originPoint axis) vx vy

placeIn :: Frame2d (global @ units) (Defines local) -> Transform2d a (local @ units) -> Transform2d a (global @ units)
placeIn frame transform = do
  let p0 = Point2d.origin |> Point2d.relativeTo frame |> Point2d.transformBy transform |> Point2d.placeIn frame
  let vx = unitX |> Vector2d.relativeTo frame |> Vector2d.transformBy transform |> Vector2d.placeIn frame
  let vy = unitY |> Vector2d.relativeTo frame |> Vector2d.transformBy transform |> Vector2d.placeIn frame
  Transform2d_ p0 vx vy

relativeTo :: Frame2d (global @ units) (Defines local) -> Transform2d a (global @ units) -> Transform2d a (local @ units)
relativeTo frame transform = do
  let p0 = Point2d.origin |> Point2d.placeIn frame |> Point2d.transformBy transform |> Point2d.relativeTo frame
  let vx = unitX |> Vector2d.placeIn frame |> Vector2d.transformBy transform |> Vector2d.relativeTo frame
  let vy = unitY |> Vector2d.placeIn frame |> Vector2d.transformBy transform |> Vector2d.relativeTo frame
  Transform2d_ p0 vx vy

fromRigid :: Rigid (space @ units) -> Transform2d a (space @ units)
fromRigid = Data.Coerce.coerce

toAffine :: Transform2d a (space @ units) -> Affine (space @ units)
toAffine = Data.Coerce.coerce

-- Helper functions to define specific/concrete transformation functions

translateByImpl :: (Rigid (space @ units) -> a -> b) -> Vector2d (space @ units) -> a -> b
translateByImpl transformBy vector = transformBy (translateBy vector)

translateByOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Vector2d (space @ units)) -> a -> b
translateByOwnImpl transformBy getVector argument =
  transformBy (translateBy (getVector argument)) argument

translateInImpl :: (Rigid (space @ units) -> a -> b) -> Direction2d space -> Qty units -> a -> b
translateInImpl transformBy direction distance = transformBy (translateIn direction distance)

translateInOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Direction2d space) -> Qty units -> a -> b
translateInOwnImpl transformBy getDirection distance argument =
  transformBy (translateIn (getDirection argument) distance) argument

translateAlongImpl :: (Rigid (space @ units) -> a -> b) -> Axis2d (space @ units) -> Qty units -> a -> b
translateAlongImpl transformBy axis distance = transformBy (translateAlong axis distance)

translateAlongOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Axis2d (space @ units)) -> Qty units -> a -> b
translateAlongOwnImpl transformBy getAxis distance argument =
  transformBy (translateAlong (getAxis argument) distance) argument

rotateAroundImpl :: (Rigid (space @ units) -> a -> b) -> Point2d (space @ units) -> Angle -> a -> b
rotateAroundImpl transformBy centerPoint angle = transformBy (rotateAround centerPoint angle)

rotateAroundOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Point2d (space @ units)) -> Angle -> a -> b
rotateAroundOwnImpl transformBy getCenterPoint angle argument =
  transformBy (rotateAround (getCenterPoint argument) angle) argument

mirrorAcrossImpl :: (Rigid (space @ units) -> a -> b) -> Axis2d (space @ units) -> a -> b
mirrorAcrossImpl transformBy axis = transformBy (mirrorAcross axis)

mirrorAcrossOwnImpl :: (Rigid (space @ units) -> a -> b) -> (a -> Axis2d (space @ units)) -> a -> b
mirrorAcrossOwnImpl transformBy getAxis argument =
  transformBy (mirrorAcross (getAxis argument)) argument

scaleAboutImpl :: (Uniform (space @ units) -> a -> b) -> Point2d (space @ units) -> Float -> a -> b
scaleAboutImpl transformBy centerPoint scale = transformBy (scaleAbout centerPoint scale)

scaleAboutOwnImpl :: (Uniform (space @ units) -> a -> b) -> (a -> Point2d (space @ units)) -> Float -> a -> b
scaleAboutOwnImpl transformBy getCenterPoint scale argument =
  transformBy (scaleAbout (getCenterPoint argument) scale) argument

scaleAlongImpl :: (Affine (space @ units) -> a -> b) -> Axis2d (space @ units) -> Float -> a -> b
scaleAlongImpl transformBy axis scale = transformBy (scaleAlong axis scale)

scaleAlongOwnImpl :: (Affine (space @ units) -> a -> b) -> (a -> Axis2d (space @ units)) -> Float -> a -> b
scaleAlongOwnImpl transformBy getAxis scale argument =
  transformBy (scaleAlong (getAxis argument) scale) argument
