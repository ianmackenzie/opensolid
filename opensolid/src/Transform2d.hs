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
  let vx = Vector2d.xy (1.0 - 2.0 * dy * dy) (2 * dx * dy)
  let vy = Vector2d.xy (2 * dx * dy) (1.0 - 2.0 * dx * dx)
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
  let xy = (scale - 1.0) * dx * dy
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

-- translateBy :: Transformable2d a space units => Vector2d (space @ units) -> a -> a
-- translateBy vector = transformBy (translateBy vector)

-- translateIn :: Transformable2d a space units => Direction2d space -> Qty units -> a -> a
-- translateIn direction distance = transformBy (translateIn direction distance)

-- translateInOwn :: Transformable2d a space units => (a -> Direction2d space) -> Qty units -> a -> a
-- translateInOwn direction distance value = translateIn (direction value) distance value

-- translateAlong :: Transformable2d a space units => Axis2d (space @ units) -> Qty units -> a -> a
-- translateAlong axis distance = transformBy (translateAlong axis distance)

-- translateAlongOwn :: Transformable2d a space units => (a -> Axis2d (space @ units)) -> Qty units -> a -> a
-- translateAlongOwn axis distance value = translateAlong (axis value) distance value

-- rotateAround :: Transformable2d a space units => Point2d (space @ units) -> Angle -> a -> a
-- rotateAround centerPoint angle = transformBy (rotateAround centerPoint angle)

-- rotateAroundOwn :: Transformable2d a space units => (a -> Point2d (space @ units)) -> Angle -> a -> a
-- rotateAroundOwn centerPoint angle value = rotateAround (centerPoint value) angle value

-- scaleAbout :: Scalable2d a space units => Point2d (space @ units) -> Float -> a -> Scaled2d a
-- scaleAbout centerPoint scale = scaleBy (scaleAbout centerPoint scale)

-- scaleAboutOwn :: Scalable2d a space units => (a -> Point2d (space @ units)) -> Float -> a -> Scaled2d a
-- scaleAboutOwn centerPoint scale value = scaleAbout (centerPoint value) scale value

-- scaleAlong :: Deformable2d a space units => Axis2d (space @ units) -> Float -> a -> Deformed2d a
-- scaleAlong axis scale = deformBy (scaleAlong axis scale)

-- scaleAlongOwn :: Deformable2d a space units => (a -> Axis2d (space @ units)) -> Float -> a -> Deformed2d a
-- scaleAlongOwn axis scale value = scaleAlong (axis value) scale value
