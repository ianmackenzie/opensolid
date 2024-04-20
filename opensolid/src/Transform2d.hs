module Transform2d
  ( Transformable2d (transformBy)
  , identity
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
import Data.Coerce qualified
import Direction2d (Direction2d)
import Direction2d qualified
import OpenSolid hiding (identity)
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified

type role Matrix phantom

data Matrix units = Matrix
  { m11 :: Float
  , m12 :: Float
  , m21 :: Float
  , m22 :: Float
  , tx :: Qty units
  , ty :: Qty units
  }

instance Units.Coercion (Matrix units1) (Matrix units2) where
  coerce = Data.Coerce.coerce

instance HasUnits (Matrix units) where
  type Units (Matrix units) = units
  type Erase (Matrix units) = Matrix Unitless

instance units ~ units' => Multiplication (Matrix units) (Matrix units') where
  type Matrix units .*. Matrix units' = Matrix (Unitless :*: units)
  Matrix{m11 = a1, m12 = b1, m21 = c1, m22 = d1, tx = x1, ty = y1} .*. Matrix{m11 = a2, m12 = b2, m21 = c2, m22 = d2, tx = x2, ty = y2} =
    Matrix
      { m11 = a1 * a2 + b1 * c2
      , m12 = a1 * b2 + b1 * d2
      , m21 = c1 * a2 + d1 * c2
      , m22 = c1 * b2 + d1 * d2
      , tx = a1 .*. x2 + b1 .*. y2 + 1.0 .*. x1
      , ty = c1 .*. x2 + d1 .*. y2 + 1.0 .*. y1
      }

instance units ~ units' => Product (Matrix units) (Matrix units') (Matrix units)

data Transformation (coordinateSystem :: CoordinateSystem) where
  Transformation :: Matrix units -> Transformation (space @ units)

instance
  (space ~ space', units ~ units') =>
  Composition
    (Transformation (space @ units))
    (Transformation (space' @ units'))
    (Transformation (space @ units))
  where
  Transformation matrix1 >> Transformation matrix2 = Transformation (matrix2 * matrix1)

identity :: Transformation (space @ units)
identity =
  Transformation (Matrix{m11 = 1.0, m12 = 0.0, m21 = 0.0, m22 = 1.0, tx = Qty.zero, ty = Qty.zero})

translationBy :: Vector2d (space @ units) -> Transformation (space @ units)
translationBy vector = do
  let (vx, vy) = Vector2d.components vector
  Transformation (Matrix{m11 = 1.0, m12 = 0.0, m21 = 0.0, m22 = 1.0, tx = vx, ty = vy})

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
  Transformation (Matrix{m11 = cos, m12 = -sin, m21 = sin, m22 = cos, tx, ty})

data Scaling (coordinateSystem :: CoordinateSystem) where
  Scaling :: Matrix units -> Scaling (space @ units)

instance
  (space ~ space', units ~ units') =>
  Composition
    (Scaling (space @ units))
    (Scaling (space' @ units'))
    (Scaling (space @ units))
  where
  Scaling matrix1 >> Scaling matrix2 = Scaling (matrix2 * matrix1)

instance
  (space ~ space', units ~ units') =>
  Composition
    (Transformation (space @ units))
    (Scaling (space' @ units'))
    (Scaling (space @ units))
  where
  Transformation matrix1 >> Scaling matrix2 = Scaling (matrix2 * matrix1)

instance
  (space ~ space', units ~ units') =>
  Composition
    (Scaling (space @ units))
    (Transformation (space' @ units'))
    (Scaling (space @ units))
  where
  Scaling matrix1 >> Transformation matrix2 = Scaling (matrix2 * matrix1)

scalingAbout :: Point2d (space @ units) -> Float -> Scaling (space @ units)
scalingAbout point scale = do
  let (cx, cy) = Point2d.coordinates point
  let tx = cx - scale * cx
  let ty = cy - scale * cy
  Scaling (Matrix{m11 = scale, m12 = 0.0, m21 = 0.0, m22 = scale, tx, ty})

data Deformation (coordinateSystem :: CoordinateSystem) where
  Deformation :: Matrix units -> Deformation (space @ units)

instance
  (space ~ space', units ~ units') =>
  Composition
    (Deformation (space @ units))
    (Deformation (space' @ units'))
    (Deformation (space @ units))
  where
  Deformation matrix1 >> Deformation matrix2 = Deformation (matrix2 * matrix1)

instance
  (space ~ space', units ~ units') =>
  Composition
    (Transformation (space @ units))
    (Deformation (space' @ units'))
    (Deformation (space @ units))
  where
  Transformation matrix1 >> Deformation matrix2 = Deformation (matrix2 * matrix1)

instance
  (space ~ space', units ~ units') =>
  Composition
    (Deformation (space @ units))
    (Transformation (space' @ units'))
    (Deformation (space @ units))
  where
  Deformation matrix1 >> Transformation matrix2 = Deformation (matrix2 * matrix1)

instance
  (space ~ space', units ~ units') =>
  Composition
    (Scaling (space @ units))
    (Deformation (space' @ units'))
    (Deformation (space @ units))
  where
  Scaling matrix1 >> Deformation matrix2 = Deformation (matrix2 * matrix1)

instance
  (space ~ space', units ~ units') =>
  Composition
    (Deformation (space @ units))
    (Scaling (space' @ units'))
    (Deformation (space @ units))
  where
  Deformation matrix1 >> Scaling matrix2 = Deformation (matrix2 * matrix1)

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
  Deformation (Matrix{m11, m12, m21, m22, tx, ty})

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
  type Scaled a
  scaleBy :: Scaling coordinateSystem -> a -> Scaled a

scaleAbout :: Scalable2d a (space @ units) => Point2d (space @ units) -> Float -> a -> Scaled a
scaleAbout centerPoint scale = scaleBy (scalingAbout centerPoint scale)

scaleAboutOwn :: Scalable2d a (space @ units) => (a -> Point2d (space @ units)) -> Float -> a -> Scaled a
scaleAboutOwn centerPoint scale value = scaleAbout (centerPoint value) scale value

class Scalable2d a coordinateSystem => Deformable2d a coordinateSystem where
  type Deformed a
  deformBy :: Deformation coordinateSystem -> a -> Deformed a

scaleAlong :: Deformable2d a (space @ units) => Axis2d (space @ units) -> Float -> a -> Deformed a
scaleAlong axis scale = deformBy (scalingAlong axis scale)

scaleAlongOwn :: Deformable2d a (space @ units) => (a -> Axis2d (space @ units)) -> Float -> a -> Deformed a
scaleAlongOwn axis scale value = scaleAlong (axis value) scale value

instance space ~ space' => Transformable2d (Direction2d space) (space' @ units') where
  transformBy transformation direction =
    Direction2d.unsafe (transformBy transformation (Direction2d.vector direction))

instance space ~ space' => Transformable2d (Vector2d (space @ units)) (space' @ units') where
  transformBy (Transformation (Matrix{m11, m12, m21, m22})) (Vector2d x y) =
    Vector2d (m11 * x + m12 * y) (m21 * x + m22 * y)

instance space ~ space' => Scalable2d (Vector2d (space @ units)) (space' @ units') where
  type Scaled (Vector2d (space @ units)) = Vector2d (space @ units)
  scaleBy (Scaling matrix) = transformBy (Transformation matrix)

instance space ~ space' => Deformable2d (Vector2d (space @ units)) (space' @ units') where
  type Deformed (Vector2d (space @ units)) = Vector2d (space @ units)
  deformBy (Deformation matrix) = transformBy (Transformation matrix)

instance (space ~ space', units ~ units') => Transformable2d (Point2d (space @ units)) (space' @ units') where
  transformBy (Transformation (Matrix{m11, m12, m21, m22, tx, ty})) point = do
    let (x, y) = Point2d.coordinates point
    Point2d.xy (m11 * x + m12 * y + tx) (m21 * x + m22 * y + ty)

instance (space ~ space', units ~ units') => Scalable2d (Point2d (space @ units)) (space' @ units') where
  type Scaled (Point2d (space @ units)) = Point2d (space @ units)
  scaleBy (Scaling matrix) = transformBy (Transformation matrix)

instance (space ~ space', units ~ units') => Deformable2d (Point2d (space @ units)) (space' @ units') where
  type Deformed (Point2d (space @ units)) = Point2d (space @ units)
  deformBy (Deformation matrix) = transformBy (Transformation matrix)
