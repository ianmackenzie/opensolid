module Transform2d
  ( Transformation2d (..)
  , Scaling2d (..)
  , Deformation2d (..)
  , Transformation
  , Scaling
  , Deformation
  , identity
  , translationBy
  , translationIn
  , translationAlong
  , rotationAround
  , scalingAbout
  )
where

import Angle qualified
import {-# SOURCE #-} Axis2d (Axis2d)
import {-# SOURCE #-} Axis2d qualified
import Direction2d (Direction2d (..))
import Direction2d qualified
import OpenSolid hiding (identity, (>>))
import Point2d (Point2d (..))
import Qty qualified
import Vector2d (Vector2d (..))

data Matrix units = Matrix Float Float Float Float (Qty units) (Qty units)

newtype Transformation coordinates units = Transformation (Matrix units)

identity :: Transformation coordinates units
identity =
  let matrix = Matrix 1.0 0.0 0.0 1.0 Qty.zero Qty.zero
   in Transformation matrix

translationBy :: Vector2d coordinates units -> Transformation coordinate units
translationBy (Vector2d vx vy) =
  let matrix = Matrix 1.0 0.0 0.0 1.0 vx vy
   in Transformation matrix

translationIn :: Direction2d coordinates -> Qty units -> Transformation coordinates units
translationIn direction distance = translationBy (direction * distance)

translationAlong :: Axis2d coordinates units -> Qty units -> Transformation coordinates units
translationAlong axis distance = translationIn (Axis2d.direction axis) distance

rotationAround :: Point2d coordinates units -> Angle -> Transformation coordinates units
rotationAround (Point2d cx cy) angle =
  let cos = Angle.cos angle
      sin = Angle.sin angle
      tx = cx - (cos * cx - sin * cy)
      ty = cy - (sin * cx + cos * cy)
      matrix = Matrix cos -sin sin cos tx ty
   in Transformation matrix

newtype Scaling coordinates units = Scaling (Matrix units)

scalingAbout :: Point2d coordinates units -> Float -> Scaling coordinates units
scalingAbout (Point2d cx cy) scale =
  let tx = cx - scale * cx
      ty = cy - scale * cy
      matrix = Matrix scale 0.0 0.0 scale tx ty
   in Scaling matrix

newtype Deformation coordinates units = Deformation (Matrix units)

class Transformation2d a coordinates units where
  transformBy :: Transformation coordinates units -> a -> a

  translateBy :: Vector2d coordinates units -> a -> a
  translateBy vector =
    transformBy (translationBy vector :: Transformation coordinates units)

  translateIn :: Direction2d coordinates -> Qty units -> a -> a
  translateIn direction distance =
    transformBy (translationIn direction distance)

  translateInOwn :: (a -> Direction2d coordinates) -> Qty units -> a -> a
  translateInOwn direction distance value =
    translateIn (direction value) distance value

  translateAlong :: Axis2d coordinates units -> Qty units -> a -> a
  translateAlong axis distance =
    transformBy (translationAlong axis distance)

  translateAlongOwn :: (a -> Axis2d coordinates units) -> Qty units -> a -> a
  translateAlongOwn axis distance value =
    translateAlong (axis value) distance value

  rotateAround :: Point2d coordinates units -> Angle -> a -> a
  rotateAround centerPoint angle =
    transformBy (rotationAround centerPoint angle)

  rotateAroundOwn :: (a -> Point2d coordinates units) -> Angle -> a -> a
  rotateAroundOwn centerPoint angle value =
    rotateAround (centerPoint value) angle value

class Transformation2d a coordinates units => Scaling2d a coordinates units where
  scaleBy :: Scaling coordinates units -> a -> a

  scaleAbout :: Point2d coordinates units -> Float -> a -> a
  scaleAbout centerPoint scale =
    scaleBy (scalingAbout centerPoint scale)

  scaleAboutOwn :: (a -> Point2d coordinates units) -> Float -> a -> a
  scaleAboutOwn centerPoint scale value = scaleAbout (centerPoint value) scale value

class Scaling2d a coordinates units => Deformation2d a coordinates units where
  deformBy :: Deformation coordinates units -> a -> a

instance
  (coordinates ~ coordinates')
  => Transformation2d (Direction2d coordinates) coordinates' units
  where
  translateBy _ vector = vector
  transformBy (Transformation (Matrix m11 m12 m21 m22 _ _)) (Direction2d x y) =
    Direction2d.unsafe (m11 * x + m12 * y) (m21 * x + m22 * y)

instance
  (coordinates ~ coordinates', units ~ units')
  => Transformation2d (Vector2d coordinates units) coordinates' units'
  where
  translateBy _ vector = vector
  transformBy (Transformation (Matrix m11 m12 m21 m22 _ _)) (Vector2d x y) =
    Vector2d (m11 * x + m12 * y) (m21 * x + m22 * y)

instance
  (coordinates ~ coordinates', units ~ units')
  => Scaling2d (Vector2d coordinates units) coordinates' units'
  where
  scaleBy (Scaling matrix) = transformBy (Transformation matrix)

instance
  (coordinates ~ coordinates', units ~ units')
  => Deformation2d (Vector2d coordinates units) coordinates' units'
  where
  deformBy (Deformation matrix) = transformBy (Transformation matrix)

instance
  (coordinates ~ coordinates', units ~ units')
  => Transformation2d (Point2d coordinates units) coordinates' units'
  where
  translateBy displacement point = point + displacement
  transformBy (Transformation (Matrix m11 m12 m21 m22 tx ty)) (Point2d x y) =
    Point2d (m11 * x + m12 * y + tx) (m21 * x + m22 * y + ty)

instance
  (coordinates ~ coordinates', units ~ units')
  => Scaling2d (Point2d coordinates units) coordinates' units'
  where
  scaleBy (Scaling matrix) = transformBy (Transformation matrix)

instance
  (coordinates ~ coordinates', units ~ units')
  => Deformation2d (Point2d coordinates units) coordinates' units'
  where
  deformBy (Deformation matrix) = transformBy (Transformation matrix)

instance units ~ units' => Multiplication (Matrix units) (Matrix units') (Matrix units) where
  Matrix b11 b12 b21 b22 bx by * Matrix a11 a12 a21 a22 ax ay =
    let m11 = b11 * a11 + b12 * a21
        m12 = b11 * a12 + b12 * a22
        m21 = b21 * a11 + b22 * a21
        m22 = b21 * a12 + b22 * a22
        x = b11 * ax + b12 * ay + bx
        y = b21 * ax + b22 * ay + by
     in Matrix m11 m12 m21 m22 x y

instance
  (coordinates ~ coordinates', units ~ units')
  => Composition (Transformation coordinates units) (Transformation coordinates' units') (Transformation coordinates units)
  where
  Transformation matrix1 >>> Transformation matrix2 = Transformation (matrix2 * matrix1)

instance
  (coordinates ~ coordinates', units ~ units')
  => Composition (Transformation coordinates units) (Scaling coordinates' units') (Scaling coordinates units)
  where
  Transformation matrix1 >>> Scaling matrix2 = Scaling (matrix2 * matrix1)

instance
  (coordinates ~ coordinates', units ~ units')
  => Composition (Transformation coordinates units) (Deformation coordinates' units') (Deformation coordinates units)
  where
  Transformation matrix1 >>> Deformation matrix2 = Deformation (matrix2 * matrix1)

instance
  (coordinates ~ coordinates', units ~ units')
  => Composition (Scaling coordinates units) (Transformation coordinates' units') (Scaling coordinates units)
  where
  Scaling matrix1 >>> Transformation matrix2 = Scaling (matrix2 * matrix1)

instance
  (coordinates ~ coordinates', units ~ units')
  => Composition (Scaling coordinates units) (Scaling coordinates' units') (Scaling coordinates units)
  where
  Scaling matrix1 >>> Scaling matrix2 = Scaling (matrix2 * matrix1)

instance
  (coordinates ~ coordinates', units ~ units')
  => Composition (Scaling coordinates units) (Deformation coordinates' units') (Deformation coordinates units)
  where
  Scaling matrix1 >>> Deformation matrix2 = Deformation (matrix2 * matrix1)

instance
  (coordinates ~ coordinates', units ~ units')
  => Composition (Deformation coordinates units) (Transformation coordinates' units') (Deformation coordinates units)
  where
  Deformation matrix1 >>> Transformation matrix2 = Deformation (matrix2 * matrix1)

instance
  (coordinates ~ coordinates', units ~ units')
  => Composition (Deformation coordinates units) (Scaling coordinates' units') (Deformation coordinates units)
  where
  Deformation matrix1 >>> Scaling matrix2 = Deformation (matrix2 * matrix1)

instance
  (coordinates ~ coordinates', units ~ units')
  => Composition (Deformation coordinates units) (Deformation coordinates' units') (Deformation coordinates units)
  where
  Deformation matrix1 >>> Deformation matrix2 = Deformation (matrix2 * matrix1)
