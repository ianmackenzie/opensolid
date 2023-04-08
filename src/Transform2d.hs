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

import Angle (Angle)
import Angle qualified
import {-# SOURCE #-} Axis2d (Axis2d)
import {-# SOURCE #-} Axis2d qualified
import CoordinateSystem (Units)
import Direction2d (Direction2d (..))
import Direction2d qualified
import OpenSolid
import Point2d (Point2d (..))
import Vector2d (Vector2d (..))

data Matrix units = Matrix Float Float Float Float (Qty units) (Qty units)

data Transformation (coordinateSystem :: CoordinateSystem) where
  Transformation :: Matrix units -> Transformation (space @ units)

translationBy :: Vector2d (space @ units) -> Transformation (space @ units)
translationBy (Vector2d vx vy) = Transformation (Matrix 1.0 0.0 0.0 1.0 vx vy)

translationIn :: Direction2d space -> Qty units -> Transformation (space @ units)
translationIn direction distance = translationBy (direction * distance)

translationAlong :: Axis2d (space @ units) -> Qty units -> Transformation (space @ units)
translationAlong axis distance = translationIn (Axis2d.direction axis) distance

rotationAround :: Point2d (space @ units) -> Angle -> Transformation (space @ units)
rotationAround (Point2d cx cy) angle =
  let cos = Angle.cos angle
      sin = Angle.sin angle
      tx = cx - (cos * cx - sin * cy)
      ty = cy - (sin * cx + cos * cy)
   in Transformation (Matrix cos -sin sin cos tx ty)

newtype Scaling (coordinateSystem :: CoordinateSystem) = Scaling (Matrix (Units coordinateSystem))

scalingAbout :: Point2d (space @ units) -> Float -> Scaling (space @ units)
scalingAbout (Point2d cx cy) scale =
  let tx = cx - scale * cx
      ty = cy - scale * cy
   in Scaling (Matrix scale 0.0 0.0 scale tx ty)

newtype Deformation (coordinateSystem :: CoordinateSystem) = Deformation (Matrix (Units coordinateSystem))

scalingAlong :: Axis2d (space @ units) -> Float -> Deformation (space @ units)
scalingAlong axis scale =
  let Direction2d dx dy = axis.direction
      Point2d x0 y0 = axis.originPoint
      dx2 = dx * dx
      dy2 = dy * dy
      m11 = scale * dx2 + dy2
      m12 = (scale - 1.0) * dx * dy
      m21 = m12
      m22 = scale * dy2 + dx2
      tx = x0 - m11 * x0 - m12 * y0
      ty = y0 - m21 * x0 - m22 * y0
   in Deformation (Matrix m11 m12 m21 m22 tx ty)

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
  transformBy (Transformation (Matrix m11 m12 m21 m22 _ _)) (Direction2d x y) =
    Direction2d.unsafe (m11 * x + m12 * y) (m21 * x + m22 * y)

instance (space ~ space', units ~ units') => Transformable2d (Vector2d (space @ units)) (space' @ units') where
  transformBy (Transformation (Matrix m11 m12 m21 m22 _ _)) (Vector2d x y) =
    Vector2d (m11 * x + m12 * y) (m21 * x + m22 * y)

instance (space ~ space', units ~ units') => Scalable2d (Vector2d (space @ units)) (space' @ units') where
  scaleBy (Scaling matrix) = transformBy (Transformation matrix)

instance (space ~ space', units ~ units') => Deformable2d (Vector2d (space @ units)) (space' @ units') where
  deformBy (Deformation matrix) = transformBy (Transformation matrix)

instance (space ~ space', units ~ units') => Transformable2d (Point2d (space @ units)) (space' @ units') where
  transformBy (Transformation (Matrix m11 m12 m21 m22 tx ty)) (Point2d x y) =
    Point2d (m11 * x + m12 * y + tx) (m21 * x + m22 * y + ty)

instance (space ~ space', units ~ units') => Scalable2d (Point2d (space @ units)) (space' @ units') where
  scaleBy (Scaling matrix) = transformBy (Transformation matrix)

instance (space ~ space', units ~ units') => Deformable2d (Point2d (space @ units)) (space' @ units') where
  deformBy (Deformation matrix) = transformBy (Transformation matrix)
