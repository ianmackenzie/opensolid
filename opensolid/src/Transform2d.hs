module Transform2d
  ( Transformable2d (transformBy)
  , translateBy
  , translateIn
  , translateInOwn
  , translateAlong
  , translateAlongOwn
  , rotateAround
  , rotateAroundOwn
  , Scalable2d (scaleBy)
  , scaleAbout
  , scaleAboutOwn
  , Deformable2d (deformBy)
  , scaleAlong
  , scaleAlongOwn
  , Transformation
  , Scaling
  , Deformation
  )
where

import Angle qualified
import {-# SOURCE #-} Axis2d (Axis2d)
import {-# SOURCE #-} Axis2d qualified
import Direction2d (Direction2d)
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified

data Matrix units = Matrix Float Float Float Float (Qty units) (Qty units)

data Transformation (coordinateSystem :: CoordinateSystem) where
  Transformation :: Matrix units -> Transformation (space @ units)

translationBy :: Vector2d (space @ units) -> Transformation (space @ units)
translationBy vector = do
  let (vx, vy) = Vector2d.components vector
  Transformation (Matrix 1.0 0.0 0.0 1.0 vx vy)

translationIn :: Direction2d space -> Qty units -> Transformation (space @ units)
translationIn direction distance = translationBy (direction * distance)

translationAlong :: Axis2d (space @ units) -> Qty units -> Transformation (space @ units)
translationAlong axis distance = translationIn (Axis2d.direction axis) distance

rotationAround :: Point2d (space @ units) -> Angle -> Transformation (space @ units)
rotationAround point angle = do
  let (cx, cy) = Point2d.coordinates point
  let cos = Angle.cos angle
  let sin = Angle.sin angle
  let tx = cx - (cos * cx - sin * cy)
  let ty = cy - (sin * cx + cos * cy)
  Transformation (Matrix cos -sin sin cos tx ty)

data Scaling (coordinateSystem :: CoordinateSystem) where
  Scaling :: Matrix units -> Scaling (space @ units)

scalingAbout :: Point2d (space @ units) -> Float -> Scaling (space @ units)
scalingAbout point scale = do
  let (cx, cy) = Point2d.coordinates point
  let tx = cx - scale * cx
  let ty = cy - scale * cy
  Scaling (Matrix scale 0.0 0.0 scale tx ty)

data Deformation (coordinateSystem :: CoordinateSystem) where
  Deformation :: Matrix units -> Deformation (space @ units)

scalingAlong :: Axis2d (space @ units) -> Float -> Deformation (space @ units)
scalingAlong axis scale = do
  let (dx, dy) = Direction2d.components (Axis2d.direction axis)
  let (x0, y0) = Point2d.coordinates (Axis2d.originPoint axis)
  let dx2 = dx * dx
  let dy2 = dy * dy
  let m11 = scale * dx2 + dy2
  let m12 = (scale - 1.0) * dx * dy
  let m21 = m12
  let m22 = scale * dy2 + dx2
  let tx = x0 - m11 * x0 - m12 * y0
  let ty = y0 - m21 * x0 - m22 * y0
  Deformation (Matrix m11 m12 m21 m22 tx ty)

class Transformable2d a (coordinateSystem :: CoordinateSystem) where
  transformBy :: Transformation coordinateSystem -> a -> a

translateBy :: Transformable2d a (space @ units) => Vector2d (space @ units) -> a -> a
translateBy vector = transformBy (translationBy vector)

translateIn :: Transformable2d a (space @ units) => Direction2d space -> Qty units -> a -> a
translateIn direction distance = transformBy (translationIn direction distance)

translateInOwn :: Transformable2d a (space @ units) => (a -> Direction2d space) -> Qty units -> a -> a
translateInOwn direction distance value = translateIn (direction value) distance value

translateAlong :: Transformable2d a (space @ units) => Axis2d (space @ units) -> Qty units -> a -> a
translateAlong axis distance = transformBy (translationAlong axis distance)

translateAlongOwn :: Transformable2d a (space @ units) => (a -> Axis2d (space @ units)) -> Qty units -> a -> a
translateAlongOwn axis distance value = translateAlong (axis value) distance value

rotateAround :: Transformable2d a (space @ units) => Point2d (space @ units) -> Angle -> a -> a
rotateAround centerPoint angle = transformBy (rotationAround centerPoint angle)

rotateAroundOwn :: Transformable2d a (space @ units) => (a -> Point2d (space @ units)) -> Angle -> a -> a
rotateAroundOwn centerPoint angle value = rotateAround (centerPoint value) angle value

class Transformable2d a coordinateSystem => Scalable2d a coordinateSystem where
  scaleBy :: Scaling coordinateSystem -> a -> a

scaleAbout :: Scalable2d a (space @ units) => Point2d (space @ units) -> Float -> a -> a
scaleAbout centerPoint scale = scaleBy (scalingAbout centerPoint scale)

scaleAboutOwn :: Scalable2d a (space @ units) => (a -> Point2d (space @ units)) -> Float -> a -> a
scaleAboutOwn centerPoint scale value = scaleAbout (centerPoint value) scale value

class Scalable2d a coordinateSystem => Deformable2d a coordinateSystem where
  deformBy :: Deformation coordinateSystem -> a -> a

scaleAlong :: Deformable2d a (space @ units) => Axis2d (space @ units) -> Float -> a -> a
scaleAlong axis scale = deformBy (scalingAlong axis scale)

scaleAlongOwn :: Deformable2d a (space @ units) => (a -> Axis2d (space @ units)) -> Float -> a -> a
scaleAlongOwn axis scale value = scaleAlong (axis value) scale value

instance space ~ space' => Transformable2d (Direction2d space) (space' @ units') where
  transformBy transformation direction =
    Direction2d.unsafe (transformBy transformation (Direction2d.unitVector direction))

instance space ~ space' => Transformable2d (Vector2d (space @ units)) (space' @ units') where
  transformBy (Transformation (Matrix m11 m12 m21 m22 _ _)) (Vector2d x y) =
    Vector2d (m11 * x + m12 * y) (m21 * x + m22 * y)

instance space ~ space' => Scalable2d (Vector2d (space @ units)) (space' @ units') where
  scaleBy (Scaling matrix) = transformBy (Transformation matrix)

instance space ~ space' => Deformable2d (Vector2d (space @ units)) (space' @ units') where
  deformBy (Deformation matrix) = transformBy (Transformation matrix)

instance (space ~ space', units ~ units') => Transformable2d (Point2d (space @ units)) (space' @ units') where
  transformBy (Transformation (Matrix m11 m12 m21 m22 tx ty)) point = do
    let (x, y) = Point2d.coordinates point
    Point2d.xy (m11 * x + m12 * y + tx) (m21 * x + m22 * y + ty)

instance (space ~ space', units ~ units') => Scalable2d (Point2d (space @ units)) (space' @ units') where
  scaleBy (Scaling matrix) = transformBy (Transformation matrix)

instance (space ~ space', units ~ units') => Deformable2d (Point2d (space @ units)) (space' @ units') where
  deformBy (Deformation matrix) = transformBy (Transformation matrix)
