module OpenSolid.Transform2d
  ( Transform2d (Transform2d)
  , Rigid
  , Orthonormal
  , Uniform
  , Affine
  , identity
  , coerce
  , handedness
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
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Float qualified as Float
import {-# SOURCE #-} OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude hiding (identity)
import OpenSolid.Primitives
  ( Axis2d (Axis2d)
  , Direction2d (Direction2d)
  , Frame2d (Frame2d)
  , Point2d (Point2d)
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
unitX = Vector2d 1.0 0.0

unitY :: Vector2d (space @ Unitless)
unitY = Vector2d 0.0 1.0

identity :: Rigid (space @ units)
identity = Transform2d Point2d.origin unitX unitY

coerce :: Transform2d tag1 (space1 @ units1) -> Transform2d tag2 (space2 @ units2)
coerce (Transform2d p0 i j) =
  Transform2d (Point2d.coerce p0) (Vector2d.coerce i) (Vector2d.coerce j)

handedness :: Transform2d tag (space @ units) -> Sign
handedness (Transform2d _ vx vy) = Float.sign (vx `cross` vy)

withFixedPoint ::
  Point2d (space @ units) ->
  Vector2d (space @ Unitless) ->
  Vector2d (space @ Unitless) ->
  Transform2d tag (space @ units)
withFixedPoint fixedPoint vx vy = do
  let Point2d fixedX fixedY = fixedPoint
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
  let vx = Vector2d cos sin
  let vy = Vector2d -sin cos
  withFixedPoint centerPoint vx vy

mirrorAcross :: Axis2d (space @ units) -> Orthonormal (space @ units)
mirrorAcross (Axis2d originPoint direction) = do
  let Direction2d dx dy = direction
  let vx = Vector2d (1.0 - 2.0 * dy * dy) (2.0 * dx * dy)
  let vy = Vector2d (2.0 * dx * dy) (1.0 - 2.0 * dx * dx)
  withFixedPoint originPoint vx vy

scaleAbout :: Point2d (space @ units) -> Float -> Uniform (space @ units)
scaleAbout point scale = do
  let vx = Vector2d scale 0.0
  let vy = Vector2d 0.0 scale
  withFixedPoint point vx vy

scaleAlong :: Axis2d (space @ units) -> Float -> Affine (space @ units)
scaleAlong (Axis2d originPoint direction) scale = do
  let Direction2d dx dy = direction
  let dx2 = dx * dx
  let dy2 = dy * dy
  let xy = (scale - 1.0) * dx * dy
  let vx = Vector2d (scale * dx2 + dy2) xy
  let vy = Vector2d xy (scale * dy2 + dx2)
  withFixedPoint originPoint vx vy

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Transform2d tag (local @ units) ->
  Transform2d tag (global @ units)
placeIn frame transform = do
  let Frame2d _ orientation = frame
  let p0 = Point2d.origin |> Point2d.relativeTo frame |> Point2d.transformBy transform |> Point2d.placeIn frame
  let vx = unitX |> Vector2d.relativeTo orientation |> Vector2d.transformBy transform |> Vector2d.placeIn orientation
  let vy = unitY |> Vector2d.relativeTo orientation |> Vector2d.transformBy transform |> Vector2d.placeIn orientation
  Transform2d p0 vx vy

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Transform2d tag (global @ units) ->
  Transform2d tag (local @ units)
relativeTo frame transform = do
  let Frame2d _ orientation = frame
  let p0 = Point2d.origin |> Point2d.placeIn frame |> Point2d.transformBy transform |> Point2d.relativeTo frame
  let vx = unitX |> Vector2d.placeIn orientation |> Vector2d.transformBy transform |> Vector2d.relativeTo orientation
  let vy = unitY |> Vector2d.placeIn orientation |> Vector2d.transformBy transform |> Vector2d.relativeTo orientation
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

translateInImpl :: (Rigid (space @ units) -> a -> b) -> Direction2d space -> Qty units -> a -> b
translateInImpl transformBy direction distance = transformBy (translateIn direction distance)

translateAlongImpl :: (Rigid (space @ units) -> a -> b) -> Axis2d (space @ units) -> Qty units -> a -> b
translateAlongImpl transformBy axis distance = transformBy (translateAlong axis distance)

rotateAroundImpl :: (Rigid (space @ units) -> a -> b) -> Point2d (space @ units) -> Angle -> a -> b
rotateAroundImpl transformBy centerPoint angle = transformBy (rotateAround centerPoint angle)

mirrorAcrossImpl :: (Orthonormal (space @ units) -> a -> b) -> Axis2d (space @ units) -> a -> b
mirrorAcrossImpl transformBy axis = transformBy (mirrorAcross axis)

scaleAboutImpl :: (Uniform (space @ units) -> a -> b) -> Point2d (space @ units) -> Float -> a -> b
scaleAboutImpl transformBy centerPoint scale = transformBy (scaleAbout centerPoint scale)

scaleAlongImpl :: (Affine (space @ units) -> a -> b) -> Axis2d (space @ units) -> Float -> a -> b
scaleAlongImpl transformBy axis scale = transformBy (scaleAlong axis scale)
