module Transform2d
  ( Transformation2d (..)
  , Transform2d
  , Rigid
  , Similarity
  , Affine
  , sequence
  , translateBy
  , rotateAround
  , translationBy
  , rotationAround
  , scaleAbout
  , scalingAbout
  )
where

import Angle qualified
import Direction2d (Direction2d (..))
import Direction2d qualified
import List qualified
import OpenSolid hiding (identity)
import Point2d (Point2d (..))
import Qty qualified
import Transformable
import Vector2d (Vector2d (..))

type Transform2d :: Type -> Type -> Type -> Type
data Transform2d category coordinates units
  = Transform2d Float Float Float Float (Qty units) (Qty units)

type Rigid coordinates units = Transform2d Transformable coordinates units

type Similarity coordinates units = Transform2d Scalable coordinates units

type Affine coordinates units = Transform2d Deformable coordinates units

class Transformation2d a category coordinates units | a -> coordinates where
  apply :: Transform2d category coordinates units -> a -> a

instance
  (coordinates ~ coordinates', units ~ units')
  => Transformation2d (Point2d coordinates units) category coordinates' units'
  where
  apply (Transform2d m11 m12 m21 m22 tx ty) (Point2d x y) =
    Point2d
      (m11 * x + m12 * y + tx)
      (m21 * x + m22 * y + ty)

instance
  (coordinates ~ coordinates', units ~ units')
  => Transformation2d (Vector2d coordinates units) category coordinates' units'
  where
  apply (Transform2d m11 m12 m21 m22 _ _) (Vector2d x y) =
    Vector2d
      (m11 * x + m12 * y)
      (m21 * x + m22 * y)

instance
  (coordinates ~ coordinates', category ~ Transformable)
  => Transformation2d (Direction2d coordinates) category coordinates' units
  where
  apply (Transform2d m11 m12 m21 m22 _ _) (Direction2d x y) =
    Direction2d.unsafe
      (m11 * x + m12 * y)
      (m21 * x + m22 * y)

sequence :: List (Transform2d category coordinates units) -> Transform2d category coordinates units
sequence (first : rest) = List.foldl compose first rest
sequence [] = identity

compose :: Transform2d category coordinates units -> Transform2d category coordinates units -> Transform2d category coordinates units
compose (Transform2d a11 a12 a21 a22 ax ay) (Transform2d b11 b12 b21 b22 bx by) =
  let m11 = b11 * a11 + b12 * a21
      m12 = b11 * a12 + b12 * a22
      m21 = b21 * a11 + b22 * a21
      m22 = b21 * a12 + b22 * a22
      tx = b11 * ax + b12 * ay + bx
      ty = b21 * ax + b22 * ay + by
   in Transform2d m11 m12 m21 m22 tx ty

identity :: Transform2d category coordinates units
identity = Transform2d 1.0 0.0 0.0 1.0 Qty.zero Qty.zero

translationBy :: Vector2d coordinates units -> Transform2d category coordinates units
translationBy (Vector2d x y) = Transform2d 1.0 0.0 0.0 1.0 x y

translateBy
  :: forall a coordinates units
   . Transformation2d a Transformable coordinates units
  => Vector2d coordinates units
  -> a
  -> a
translateBy vector = apply (translationBy vector :: Transform2d Transformable coordinates units)

rotationAround :: Point2d coordinates units -> Angle -> Transform2d category coordinates units
rotationAround (Point2d cx cy) angle =
  let cos = Angle.cos angle
      sin = Angle.sin angle
      tx = cx - (cos * cx - sin * cy)
      ty = cy - (sin * cx + cos * cy)
   in Transform2d cos -sin sin cos tx ty

rotateAround
  :: forall a coordinates units
   . Transformation2d a Transformable coordinates units
  => Point2d coordinates units
  -> Angle
  -> a
  -> a
rotateAround point angle = apply (rotationAround point angle :: Transform2d Transformable coordinates units)

scalingAbout :: IsScalable category => Point2d coordinates units -> Float -> Transform2d category coordinates units
scalingAbout (Point2d cx cy) scale =
  let tx = cx - scale * cx
      ty = cy - scale * cy
   in Transform2d scale 0.0 0.0 scale tx ty

scaleAbout
  :: forall a coordinates units
   . Transformation2d a Scalable coordinates units
  => Point2d coordinates units
  -> Float
  -> a
  -> a
scaleAbout point scale = apply (scalingAbout point scale :: Transform2d Scalable coordinates units)
