module OpenSolid.Transform2D
  ( Transform2D (Transform2D)
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
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2D (Axis2D)
  , Direction2D (Direction2D)
  , Frame2D
  , Transform2D (Transform2D)
  , Vector2D (Vector2D)
  )
import OpenSolid.Transform qualified as Transform

type Rigid units space = Transform2D Transform.Rigid units space

type Orthonormal units space = Transform2D Transform.Orthonormal units space

type Uniform units space = Transform2D Transform.Uniform units space

type Affine units space = Transform2D Transform.Affine units space

unitX :: Vector2D Unitless space
unitX = Vector2D 1 0

unitY :: Vector2D Unitless space
unitY = Vector2D 0 1

identity :: Rigid units space
identity = Transform2D Point2D.origin unitX unitY

coerce :: Transform2D tag1 units1 space1 -> Transform2D tag2 units2 space2
coerce (Transform2D p0 i j) =
  Transform2D (Point2D.coerce p0) (Vector2D.coerce i) (Vector2D.coerce j)

handedness :: Transform2D tag units space -> Sign
handedness (Transform2D _ vx vy) = Number.sign (vx `cross` vy)

withFixedPoint ::
  Point2D units space ->
  Vector2D Unitless space ->
  Vector2D Unitless space ->
  Transform2D tag units space
withFixedPoint fixedPoint vx vy = do
  let Point2D fixedX fixedY = fixedPoint
  let originPoint = fixedPoint .-. fixedX .*. vx .-. fixedY .*. vy
  Transform2D originPoint vx vy

translateBy :: Vector2D units space -> Rigid units space
translateBy vector = Transform2D (Point2D.origin .+. vector) unitX unitY

translateIn :: Direction2D space -> Quantity units -> Rigid units space
translateIn direction distance = translateBy (direction .*. distance)

translateAlong :: Axis2D units space -> Quantity units -> Rigid units space
translateAlong (Axis2D _ direction) distance = translateIn direction distance

rotateAround :: Point2D units space -> Angle -> Rigid units space
rotateAround centerPoint angle = do
  let cos = Angle.cos angle
  let sin = Angle.sin angle
  let vx = Vector2D cos sin
  let vy = Vector2D -sin cos
  withFixedPoint centerPoint vx vy

mirrorAcross :: Axis2D units space -> Orthonormal units space
mirrorAcross (Axis2D originPoint direction) = do
  let Direction2D dx dy = direction
  let vx = Vector2D (1 -. 2 *. dy *. dy) (2 *. dx *. dy)
  let vy = Vector2D (2 *. dx *. dy) (1 -. 2 *. dx *. dx)
  withFixedPoint originPoint vx vy

scaleAbout :: Point2D units space -> Number -> Uniform units space
scaleAbout point scale = do
  let vx = Vector2D scale 0
  let vy = Vector2D 0 scale
  withFixedPoint point vx vy

scaleAlong :: Axis2D units space -> Number -> Affine units space
scaleAlong (Axis2D originPoint direction) scale = do
  let Direction2D dx dy = direction
  let dx2 = dx .*. dx
  let dy2 = dy .*. dy
  let xy = (scale .- 1) .*. dx .*. dy
  let vx = Vector2D (scale .*. dx2 .+. dy2) xy
  let vy = Vector2D xy (scale .*. dy2 .+. dx2)
  withFixedPoint originPoint vx vy

placeIn ::
  Frame2D units global local ->
  Transform2D tag units local ->
  Transform2D tag units global
placeIn frame transform = do
  let p0 =
        Point2D.origin
          & Point2D.relativeTo frame
          & Point2D.transformBy transform
          & Point2D.placeIn frame
  let vx =
        unitX
          & Vector2D.relativeTo frame
          & Vector2D.transformBy transform
          & Vector2D.placeIn frame
  let vy =
        unitY
          & Vector2D.relativeTo frame
          & Vector2D.transformBy transform
          & Vector2D.placeIn frame
  Transform2D p0 vx vy

relativeTo ::
  Frame2D units global local ->
  Transform2D tag units global ->
  Transform2D tag units local
relativeTo frame transform = do
  let p0 =
        Point2D.origin
          & Point2D.placeIn frame
          & Point2D.transformBy transform
          & Point2D.relativeTo frame
  let vx =
        unitX
          & Vector2D.placeIn frame
          & Vector2D.transformBy transform
          & Vector2D.relativeTo frame
  let vy =
        unitY
          & Vector2D.placeIn frame
          & Vector2D.transformBy transform
          & Vector2D.relativeTo frame
  Transform2D p0 vx vy

toOrthonormal ::
  Transform.IsOrthonormal tag =>
  Transform2D tag units space ->
  Orthonormal units space
toOrthonormal = Data.Coerce.coerce

toUniform ::
  Transform.IsUniform tag =>
  Transform2D tag units space ->
  Uniform units space
toUniform = Data.Coerce.coerce

toAffine :: Transform2D tag units space -> Affine units space
toAffine = Data.Coerce.coerce

-- Helper functions to define specific/concrete transformation functions

translateByImpl ::
  (Rigid units space -> a -> b) ->
  Vector2D units space ->
  a ->
  b
translateByImpl transformBy vector = transformBy (translateBy vector)

translateInImpl ::
  (Rigid units space -> a -> b) ->
  Direction2D space ->
  Quantity units ->
  a ->
  b
translateInImpl transformBy direction distance = transformBy (translateIn direction distance)

translateAlongImpl ::
  (Rigid units space -> a -> b) ->
  Axis2D units space ->
  Quantity units ->
  a ->
  b
translateAlongImpl transformBy axis distance = transformBy (translateAlong axis distance)

rotateAroundImpl ::
  (Rigid units space -> a -> b) ->
  Point2D units space ->
  Angle ->
  a ->
  b
rotateAroundImpl transformBy centerPoint angle = transformBy (rotateAround centerPoint angle)

mirrorAcrossImpl ::
  (Orthonormal units space -> a -> b) ->
  Axis2D units space ->
  a ->
  b
mirrorAcrossImpl transformBy axis = transformBy (mirrorAcross axis)

scaleAboutImpl ::
  (Uniform units space -> a -> b) ->
  Point2D units space ->
  Number ->
  a ->
  b
scaleAboutImpl transformBy centerPoint scale = transformBy (scaleAbout centerPoint scale)

scaleAlongImpl ::
  (Affine units space -> a -> b) ->
  Axis2D units space ->
  Number ->
  a ->
  b
scaleAlongImpl transformBy axis scale = transformBy (scaleAlong axis scale)
