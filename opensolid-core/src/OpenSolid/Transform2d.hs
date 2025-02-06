module OpenSolid.Transform2d
  ( Transform2d (Transform2d)
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
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude hiding (identity)
import OpenSolid.Primitives
  ( Axis2d (Axis2d)
  , Direction2d (Unit2d)
  , Frame2d
  , Point2d
  , Transform2d (Transform2d)
  , Vector2d (Vector2d)
  )
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector2d qualified as Vector2d

type Rigid coordinateSystem = Transform2d Transform.Rigid coordinateSystem

type Orthonormal coordinateSystem = Transform2d Transform.Orthonormal coordinateSystem

type Uniform coordinateSystem = Transform2d Transform.Uniform coordinateSystem

type Affine coordinateSystem = Transform2d Transform.Affine coordinateSystem

unitX :: Vector2d (space @ Unitless)
unitX = Vector2d.xy 1.0 0.0

unitY :: Vector2d (space @ Unitless)
unitY = Vector2d.xy 0.0 1.0

identity :: Rigid (space @ units)
identity = Transform2d Point2d.origin unitX unitY

withFixedPoint ::
  Point2d (space @ units) ->
  Vector2d (space @ Unitless) ->
  Vector2d (space @ Unitless) ->
  Transform2d tag (space @ units)
withFixedPoint fixedPoint vx vy = do
  let (fixedX, fixedY) = Point2d.coordinates fixedPoint
  Transform2d (fixedPoint - fixedX * vx - fixedY * vy) vx vy

translateBy :: Vector2d (space @ units) -> Rigid (space @ units)
translateBy vector = Transform2d (Point2d.origin + vector) unitX unitY

translateIn :: Direction2d space -> Qty units -> Rigid (space @ units)
translateIn direction distance = translateBy (direction * distance)

translateAlong :: Axis2d (space @ units) -> Qty units -> Rigid (space @ units)
translateAlong (Axis2d _ direction) distance = translateIn direction distance

rotateAround :: Point2d (space @ units) -> Angle -> Rigid (space @ units)
rotateAround centerPoint angle = do
  let cos = Angle.cos angle
  let sin = Angle.sin angle
  let vx = Vector2d.xy cos sin
  let vy = Vector2d.xy -sin cos
  withFixedPoint centerPoint vx vy

mirrorAcross :: Axis2d (space @ units) -> Orthonormal (space @ units)
mirrorAcross (Axis2d originPoint direction) = do
  let Unit2d (Vector2d dx dy) = direction
  let vx = Vector2d.xy (1.0 - 2.0 * dy * dy) (2.0 * dx * dy)
  let vy = Vector2d.xy (2.0 * dx * dy) (1.0 - 2.0 * dx * dx)
  withFixedPoint originPoint vx vy

scaleAbout :: Point2d (space @ units) -> Float -> Uniform (space @ units)
scaleAbout point scale = do
  let vx = Vector2d.xy scale 0.0
  let vy = Vector2d.xy 0.0 scale
  withFixedPoint point vx vy

scaleAlong :: Axis2d (space @ units) -> Float -> Affine (space @ units)
scaleAlong (Axis2d originPoint direction) scale = do
  let Unit2d (Vector2d dx dy) = direction
  let dx2 = dx * dx
  let dy2 = dy * dy
  let xy = (scale - 1.0) * dx * dy
  let vx = Vector2d.xy (scale * dx2 + dy2) xy
  let vy = Vector2d.xy xy (scale * dy2 + dx2)
  withFixedPoint originPoint vx vy

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Transform2d tag (local @ units) ->
  Transform2d tag (global @ units)
placeIn frame transform = do
  let p0 = Point2d.origin |> Point2d.relativeTo frame |> Point2d.transformBy transform |> Point2d.placeIn frame
  let vx = unitX |> Vector2d.relativeTo frame |> Vector2d.transformBy transform |> Vector2d.placeIn frame
  let vy = unitY |> Vector2d.relativeTo frame |> Vector2d.transformBy transform |> Vector2d.placeIn frame
  Transform2d p0 vx vy

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Transform2d tag (global @ units) ->
  Transform2d tag (local @ units)
relativeTo frame transform = do
  let p0 = Point2d.origin |> Point2d.placeIn frame |> Point2d.transformBy transform |> Point2d.relativeTo frame
  let vx = unitX |> Vector2d.placeIn frame |> Vector2d.transformBy transform |> Vector2d.relativeTo frame
  let vy = unitY |> Vector2d.placeIn frame |> Vector2d.transformBy transform |> Vector2d.relativeTo frame
  Transform2d p0 vx vy

toOrthonormal :: Transform.IsOrthonormal tag => Transform2d tag (space @ units) -> Orthonormal (space @ units)
toOrthonormal = Data.Coerce.coerce

toUniform :: Transform.IsUniform tag => Transform2d tag (space @ units) -> Uniform (space @ units)
toUniform = Data.Coerce.coerce

toAffine :: Transform2d tag (space @ units) -> Affine (space @ units)
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

mirrorAcrossImpl :: (Orthonormal (space @ units) -> a -> b) -> Axis2d (space @ units) -> a -> b
mirrorAcrossImpl transformBy axis = transformBy (mirrorAcross axis)

mirrorAcrossOwnImpl :: (Orthonormal (space @ units) -> a -> b) -> (a -> Axis2d (space @ units)) -> a -> b
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
