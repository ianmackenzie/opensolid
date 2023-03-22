module Transform2d
  ( Transformable2d (..)
  , Scalable2d (..)
  , Deformable2d (..)
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
import OpenSolid hiding (identity)
import Point2d (Point2d (..))
import Vector2d (Vector2d (..))

data Matrix units = Matrix Float Float Float Float (Qty units) (Qty units)

newtype Transformation (coordinateSystem :: CoordinateSystem) = Transformation (Matrix (Units coordinateSystem))

translationBy :: Vector2d (Coordinates space units) -> Transformation (Coordinates space units)
translationBy (Vector2d vx vy) = Transformation (Matrix 1.0 0.0 0.0 1.0 vx vy)

translationIn :: Direction2d space -> Qty units -> Transformation (Coordinates space units)
translationIn direction distance = translationBy (direction * distance)

translationAlong :: Axis2d (Coordinates space units) -> Qty units -> Transformation (Coordinates space units)
translationAlong axis distance = translationIn (Axis2d.direction axis) distance

rotationAround :: Point2d (Coordinates space units) -> Angle -> Transformation (Coordinates space units)
rotationAround (Point2d cx cy) angle =
  let cos = Angle.cos angle
      sin = Angle.sin angle
      tx = cx - (cos * cx - sin * cy)
      ty = cy - (sin * cx + cos * cy)
   in Transformation (Matrix cos -sin sin cos tx ty)

newtype Scaling (coordinateSystem :: CoordinateSystem) = Scaling (Matrix (Units coordinateSystem))

scalingAbout :: Point2d (Coordinates space units) -> Float -> Scaling (Coordinates space units)
scalingAbout (Point2d cx cy) scale =
  let tx = cx - scale * cx
      ty = cy - scale * cy
   in Scaling (Matrix scale 0.0 0.0 scale tx ty)

newtype Deformation (coordinateSystem :: CoordinateSystem) = Deformation (Matrix (Units coordinateSystem))

scalingAlong :: Axis2d (Coordinates space units) -> Float -> Deformation (Coordinates space units)
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

class Transformable2d a space units where
  transformBy :: Transformation (Coordinates space units) -> a -> a

  translateBy :: Vector2d (Coordinates space units) -> a -> a
  translateBy vector = transformBy (translationBy vector)

  translateIn :: Direction2d space -> Qty units -> a -> a
  translateIn direction distance = transformBy (translationIn direction distance)

  translateInOwn :: (a -> Direction2d space) -> Qty units -> a -> a
  translateInOwn direction distance value = translateIn (direction value) distance value

  translateAlong :: Axis2d (Coordinates space units) -> Qty units -> a -> a
  translateAlong axis distance = transformBy (translationAlong axis distance)

  translateAlongOwn :: (a -> Axis2d (Coordinates space units)) -> Qty units -> a -> a
  translateAlongOwn axis distance value = translateAlong (axis value) distance value

  rotateAround :: Point2d (Coordinates space units) -> Angle -> a -> a
  rotateAround centerPoint angle = transformBy (rotationAround centerPoint angle)

  rotateAroundOwn :: (a -> Point2d (Coordinates space units)) -> Angle -> a -> a
  rotateAroundOwn centerPoint angle value = rotateAround (centerPoint value) angle value

class Transformable2d a space units => Scalable2d a space units where
  scaleBy :: Scaling (Coordinates space units) -> a -> a

  scaleAbout :: Point2d (Coordinates space units) -> Float -> a -> a
  scaleAbout centerPoint scale = scaleBy (scalingAbout centerPoint scale)

  scaleAboutOwn :: (a -> Point2d (Coordinates space units)) -> Float -> a -> a
  scaleAboutOwn centerPoint scale value = scaleAbout (centerPoint value) scale value

class Scalable2d a space units => Deformable2d a space units where
  deformBy :: Deformation (Coordinates space units) -> a -> a

  scaleAlong :: Axis2d (Coordinates space units) -> Float -> a -> a
  scaleAlong axis scale = deformBy (scalingAlong axis scale)

instance space ~ space' => Transformable2d (Direction2d space) space' units' where
  translateBy _ vector = vector
  transformBy (Transformation (Matrix m11 m12 m21 m22 _ _)) (Direction2d x y) =
    Direction2d.unsafe (m11 * x + m12 * y) (m21 * x + m22 * y)

instance (space ~ space', units ~ units') => Transformable2d (Vector2d (Coordinates space units)) space' units' where
  translateBy _ vector = vector
  transformBy (Transformation (Matrix m11 m12 m21 m22 _ _)) (Vector2d x y) =
    Vector2d (m11 * x + m12 * y) (m21 * x + m22 * y)

instance (space ~ space', units ~ units') => Scalable2d (Vector2d (Coordinates space units)) space' units' where
  scaleBy (Scaling matrix) = transformBy (Transformation matrix)

instance (space ~ space', units ~ units') => Deformable2d (Vector2d (Coordinates space units)) space' units' where
  deformBy (Deformation matrix) = transformBy (Transformation matrix)

instance (space ~ space', units ~ units') => Transformable2d (Point2d (Coordinates space units)) space' units' where
  translateBy displacement point = point + displacement
  transformBy (Transformation (Matrix m11 m12 m21 m22 tx ty)) (Point2d x y) =
    Point2d (m11 * x + m12 * y + tx) (m21 * x + m22 * y + ty)

instance (space ~ space', units ~ units') => Scalable2d (Point2d (Coordinates space units)) space' units' where
  scaleBy (Scaling matrix) = transformBy (Transformation matrix)

instance (space ~ space', units ~ units') => Deformable2d (Point2d (Coordinates space units)) space' units' where
  deformBy (Deformation matrix) = transformBy (Transformation matrix)
