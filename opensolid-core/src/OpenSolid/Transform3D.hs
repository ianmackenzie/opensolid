module OpenSolid.Transform3D
  ( Transform3D (Transform3D)
  , Rigid
  , Orthonormal
  , Uniform
  , Affine
  , vectorTransform
  , identity
  , coerce
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , scaleAbout
  , scaleAlong
  , placeIn
  , relativeTo
  , asOrthonormal
  , asUniform
  , asAffine
  , isRigid
  , isOrthonormal
  , isUniform
  , toRigid
  , toOrthonormal
  , toUniform
  , handedness
  , scale
  , orthonormalSign
  , uniformScale
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
import OpenSolid.Length (Length)
import {-# SOURCE #-} OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D (..)
  , Direction3D
  , Frame3D
  , Plane3D (Plane3D)
  , Point3D (Point3D, Position3D)
  , Transform3D (Transform3D)
  , Vector3D
  , VectorTransform3D (VectorTransform3D)
  )
import {-# SOURCE #-} OpenSolid.Transform qualified as Transform
import OpenSolid.VectorTransform3D qualified as VectorTransform3D
import OpenSolid.World3D qualified as World3D

type Rigid space = Transform3D Transform.Rigid space

type Orthonormal space = Transform3D Transform.Orthonormal space

type Uniform space = Transform3D Transform.Uniform space

type Affine space = Transform3D Transform.Affine space

{-# INLINE vectorTransform #-}
vectorTransform :: Transform3D tag space -> VectorTransform3D tag space
vectorTransform (Transform3D _ vt) = vt

identity :: Rigid space
identity = Transform3D World3D.originPoint VectorTransform3D.identity

coerce :: Transform3D tag1 space1 -> Transform3D tag2 space2
coerce (Transform3D p0 vt) = Transform3D (Point3D.coerce p0) (VectorTransform3D.coerce vt)

withFixedPoint :: Point3D space -> VectorTransform3D tag space -> Transform3D tag space
withFixedPoint fixedPoint givenVectorTransform = do
  let VectorTransform3D vx vy vz = givenVectorTransform
  let Point3D x0 y0 z0 = fixedPoint
  let originPoint = fixedPoint - x0 * vx - y0 * vy - z0 * vz
  Transform3D originPoint givenVectorTransform

translateBy :: Vector3D Meters space -> Rigid space
translateBy vector = Transform3D (Position3D vector) VectorTransform3D.identity

translateIn :: Direction3D space -> Length -> Rigid space
translateIn direction distance = translateBy (direction * distance)

translateAlong :: Axis3D space -> Length -> Rigid space
translateAlong axis distance = translateIn axis.direction distance

rotateAround :: Axis3D space -> Angle -> Rigid space
rotateAround axis angle =
  withFixedPoint axis.originPoint (VectorTransform3D.rotateAround axis angle)

scaleAbout :: Point3D space -> Number -> Uniform space
scaleAbout point givenScale =
  withFixedPoint point (VectorTransform3D.scaleBy givenScale)

scaleAlong :: Axis3D space -> Number -> Affine space
scaleAlong axis givenScale =
  withFixedPoint axis.originPoint (VectorTransform3D.scaleAlong axis givenScale)

mirrorAcross :: Plane3D space -> Orthonormal space
mirrorAcross plane = do
  let Plane3D originPoint _ = plane
  withFixedPoint originPoint (VectorTransform3D.mirrorAcross plane)

placeIn :: Frame3D global local -> Transform3D tag local -> Transform3D tag global
placeIn frame transform =
  Transform3D
    (World3D.originPoint / frame * transform * frame)
    (VectorTransform3D.placeIn frame (vectorTransform transform))

relativeTo :: Frame3D global local -> Transform3D tag global -> Transform3D tag local
relativeTo frame transform =
  Transform3D
    (World3D.originPoint * frame * transform / frame)
    (VectorTransform3D.relativeTo frame (vectorTransform transform))

asOrthonormal :: Transform.IsOrthonormal tag => Transform3D tag space -> Orthonormal space
asOrthonormal = Data.Coerce.coerce

asUniform :: Transform.IsUniform tag => Transform3D tag space -> Uniform space
asUniform = Data.Coerce.coerce

asAffine :: Transform3D tag space -> Affine space
asAffine = Data.Coerce.coerce

handedness :: Transform3D tag space -> Sign
handedness = VectorTransform3D.handedness . vectorTransform

scale :: Uniform space -> Number
scale = VectorTransform3D.scale . vectorTransform

isRigid :: Transform3D tag space -> Bool
isRigid = VectorTransform3D.isRigid . vectorTransform

isOrthonormal :: Transform3D tag space -> Bool
isOrthonormal = VectorTransform3D.isOrthonormal . vectorTransform

isUniform :: Transform3D tag space -> Bool
isUniform = VectorTransform3D.isUniform . vectorTransform

orthonormalSign :: Transform3D tag space -> Maybe Sign
orthonormalSign = VectorTransform3D.orthonormalSign . vectorTransform

uniformScale :: Transform3D tag space -> Maybe Number
uniformScale = VectorTransform3D.uniformScale . vectorTransform

toRigid :: Transform3D tag space -> Maybe (Rigid space)
toRigid transform = if isRigid transform then Just (coerce transform) else Nothing

toOrthonormal :: Transform3D tag space -> Maybe (Orthonormal space)
toOrthonormal transform = if isOrthonormal transform then Just (coerce transform) else Nothing

toUniform :: Transform3D tag space -> Maybe (Uniform space)
toUniform transform = if isUniform transform then Just (coerce transform) else Nothing

-- Helper functions to define specific/concrete transformation functions

translateByImpl :: (Rigid space -> a -> b) -> Vector3D Meters space -> a -> b
translateByImpl transformBy vector = transformBy (translateBy vector)

translateInImpl :: (Rigid space -> a -> b) -> Direction3D space -> Length -> a -> b
translateInImpl transformBy direction distance = transformBy (translateIn direction distance)

translateAlongImpl :: (Rigid space -> a -> b) -> Axis3D space -> Length -> a -> b
translateAlongImpl transformBy axis distance = transformBy (translateAlong axis distance)

rotateAroundImpl :: (Rigid space -> a -> b) -> Axis3D space -> Angle -> a -> b
rotateAroundImpl transformBy axis angle = transformBy (rotateAround axis angle)

mirrorAcrossImpl :: (Orthonormal space -> a -> b) -> Plane3D space -> a -> b
mirrorAcrossImpl transformBy plane = transformBy (mirrorAcross plane)

scaleAboutImpl :: (Uniform space -> a -> b) -> Point3D space -> Number -> a -> b
scaleAboutImpl transformBy centerPoint givenScale = transformBy (scaleAbout centerPoint givenScale)

scaleAlongImpl :: (Affine space -> a -> b) -> Axis3D space -> Number -> a -> b
scaleAlongImpl transformBy axis givenScale = transformBy (scaleAlong axis givenScale)
