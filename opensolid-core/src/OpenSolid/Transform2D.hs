module OpenSolid.Transform2D
  ( Transform2D
  , Rigid
  , Orthonormal
  , Uniform
  , Affine
  , vectorTransform
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
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2D (..)
  , Frame2D
  , Point2D (Point2D, Position2D)
  , Transform2D (Transform2D)
  , Vector2D
  , VectorTransform2D (VectorTransform2D)
  )
import {-# SOURCE #-} OpenSolid.Transform qualified as Transform
import OpenSolid.VectorTransform2D qualified as VectorTransform2D

type Rigid units = Transform2D Transform.Rigid units

type Orthonormal units = Transform2D Transform.Orthonormal units

type Uniform units = Transform2D Transform.Uniform units

type Affine units = Transform2D Transform.Affine units

{-# INLINE vectorTransform #-}
vectorTransform :: Transform2D tag units -> VectorTransform2D tag
vectorTransform (Transform2D _ vt) = vt

identity :: Rigid units
identity = Transform2D Point2D.origin VectorTransform2D.identity

coerce :: Transform2D tag1 units1 -> Transform2D tag2 units2
coerce (Transform2D p0 vt) = Transform2D (Point2D.coerce p0) (VectorTransform2D.coerce vt)

handedness :: Transform2D tag units -> Sign
handedness = VectorTransform2D.handedness . vectorTransform

withFixedPoint :: Point2D units -> VectorTransform2D tag -> Transform2D tag units
withFixedPoint fixedPoint givenVectorTransform = do
  let VectorTransform2D vx vy = givenVectorTransform
  let Point2D fixedX fixedY = fixedPoint
  let originPoint = fixedPoint - fixedX * vx - fixedY * vy
  Transform2D originPoint givenVectorTransform

translateBy :: Vector2D units -> Rigid units
translateBy vector = Transform2D (Position2D vector) VectorTransform2D.identity

translateIn :: Direction2D -> Quantity units -> Rigid units
translateIn direction distance = translateBy (direction * distance)

translateAlong :: Axis2D units -> Quantity units -> Rigid units
translateAlong axis distance = translateIn axis.direction distance

rotateAround :: Point2D units -> Angle -> Rigid units
rotateAround centerPoint angle = withFixedPoint centerPoint (VectorTransform2D.rotateBy angle)

mirrorAcross :: Axis2D units -> Orthonormal units
mirrorAcross axis = withFixedPoint axis.originPoint (VectorTransform2D.mirrorAcross axis)

scaleAbout :: Point2D units -> Number -> Uniform units
scaleAbout point scale = withFixedPoint point (VectorTransform2D.scaleBy scale)

scaleAlong :: Axis2D units -> Number -> Affine units
scaleAlong axis scale = withFixedPoint axis.originPoint (VectorTransform2D.scaleAlong axis scale)

placeIn :: Frame2D units -> Transform2D tag units -> Transform2D tag units
placeIn frame transform = do
  let p0 = Point2D.origin / frame * transform * frame
  Transform2D p0 (VectorTransform2D.placeIn frame (vectorTransform transform))

relativeTo :: Frame2D units -> Transform2D tag units -> Transform2D tag units
relativeTo frame transform = do
  let p0 = Point2D.origin * frame * transform / frame
  Transform2D p0 (VectorTransform2D.relativeTo frame (vectorTransform transform))

toOrthonormal :: Transform.IsOrthonormal tag => Transform2D tag units -> Orthonormal units
toOrthonormal = Data.Coerce.coerce

toUniform :: Transform.IsUniform tag => Transform2D tag units -> Uniform units
toUniform = Data.Coerce.coerce

toAffine :: Transform2D tag units -> Affine units
toAffine = Data.Coerce.coerce

-- Helper functions to define specific/concrete transformation functions

translateByImpl :: (Rigid units -> a -> b) -> Vector2D units -> a -> b
translateByImpl transformBy vector = transformBy (translateBy vector)

translateInImpl :: (Rigid units -> a -> b) -> Direction2D -> Quantity units -> a -> b
translateInImpl transformBy direction distance = transformBy (translateIn direction distance)

translateAlongImpl :: (Rigid units -> a -> b) -> Axis2D units -> Quantity units -> a -> b
translateAlongImpl transformBy axis distance = transformBy (translateAlong axis distance)

rotateAroundImpl :: (Rigid units -> a -> b) -> Point2D units -> Angle -> a -> b
rotateAroundImpl transformBy centerPoint angle = transformBy (rotateAround centerPoint angle)

mirrorAcrossImpl :: (Orthonormal units -> a -> b) -> Axis2D units -> a -> b
mirrorAcrossImpl transformBy axis = transformBy (mirrorAcross axis)

scaleAboutImpl :: (Uniform units -> a -> b) -> Point2D units -> Number -> a -> b
scaleAboutImpl transformBy centerPoint scale = transformBy (scaleAbout centerPoint scale)

scaleAlongImpl :: (Affine units -> a -> b) -> Axis2D units -> Number -> a -> b
scaleAlongImpl transformBy axis scale = transformBy (scaleAlong axis scale)
