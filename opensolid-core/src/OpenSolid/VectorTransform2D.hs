module OpenSolid.VectorTransform2D
  ( VectorTransform2D
  , Rigid
  , Orthonormal
  , Uniform
  , Affine
  , identity
  , coerce
  , erase
  , unerase
  , scaleBy
  , scaleIn
  , scaleAlong
  , rotateBy
  , mirrorIn
  , mirrorAcross
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
  , scaleInImpl
  , scaleAlongImpl
  , rotateByImpl
  , mirrorInImpl
  , mirrorAcrossImpl
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2D (..)
  , Direction2D (Direction2D)
  , Frame2D
  , Vector2D (Vector2D)
  , VectorTransform2D (VectorTransform2D)
  )
import OpenSolid.Tolerance qualified as Tolerance
import {-# SOURCE #-} OpenSolid.Transform qualified as Transform
import {-# SOURCE #-} OpenSolid.Vector2D qualified as Vector2D

type Rigid = VectorTransform2D Transform.Rigid

type Orthonormal = VectorTransform2D Transform.Orthonormal

type Uniform = VectorTransform2D Transform.Uniform

type Affine = VectorTransform2D Transform.Affine

unitX :: Vector2D Unitless
unitX = Vector2D 1.0 0.0

unitY :: Vector2D Unitless
unitY = Vector2D 0.0 1.0

identity :: Rigid
identity = VectorTransform2D unitX unitY

{-# INLINE coerce #-}
coerce :: VectorTransform2D tag1 -> VectorTransform2D tag2
coerce = Data.Coerce.coerce

{-# INLINE erase #-}
erase :: VectorTransform2D tag -> Affine
erase = coerce

{-# INLINE unerase #-}
unerase :: Affine -> VectorTransform2D tag
unerase = coerce

scaleBy :: Number -> Uniform
scaleBy givenScale = VectorTransform2D (Vector2D givenScale 0.0) (Vector2D 0.0 givenScale)

scaleIn :: Direction2D -> Number -> Affine
scaleIn (Direction2D dx dy) givenScale = do
  let dx2 = dx * dx
  let dy2 = dy * dy
  let xy = (givenScale - 1.0) * dx * dy
  let vx = Vector2D (givenScale * dx2 + dy2) xy
  let vy = Vector2D xy (givenScale * dy2 + dx2)
  VectorTransform2D vx vy

scaleAlong :: Axis2D units -> Number -> Affine
scaleAlong (Axis2D _ direction) givenScale = scaleIn direction givenScale

rotateBy :: Angle -> Rigid
rotateBy angle = do
  let cos = Angle.cos angle
  let sin = Angle.sin angle
  let vx = Vector2D cos sin
  let vy = Vector2D -sin cos
  VectorTransform2D vx vy

mirrorIn :: Direction2D -> Orthonormal
mirrorIn (Direction2D dx dy) = do
  let vx = Vector2D (1.0 - 2.0 * dx * dx) (-2.0 * dx * dy)
  let vy = Vector2D (-2.0 * dx * dy) (1.0 - 2.0 * dy * dy)
  VectorTransform2D vx vy

mirrorAcross :: Axis2D units -> Orthonormal
mirrorAcross axis = do
  let Direction2D dx dy = axis.direction
  mirrorIn (Direction2D -dy dx)

placeIn :: Frame2D units -> VectorTransform2D tag -> VectorTransform2D tag
placeIn frame transform = do
  let vx = unitX / frame * transform * frame
  let vy = unitY / frame * transform * frame
  VectorTransform2D vx vy

relativeTo :: Frame2D units -> VectorTransform2D tag -> VectorTransform2D tag
relativeTo frame transform = do
  let vx = unitX * frame * transform / frame
  let vy = unitY * frame * transform / frame
  VectorTransform2D vx vy

asOrthonormal :: Transform.IsOrthonormal tag => VectorTransform2D tag -> Orthonormal
asOrthonormal = Data.Coerce.coerce

asUniform :: Transform.IsUniform tag => VectorTransform2D tag -> Uniform
asUniform = Data.Coerce.coerce

asAffine :: VectorTransform2D tag -> Affine
asAffine = Data.Coerce.coerce

handedness :: VectorTransform2D tag -> Sign
handedness (VectorTransform2D vx vy) = Number.sign (vx `cross` vy)

scale :: Uniform -> Number
scale (VectorTransform2D vx _) = Vector2D.magnitude vx

isRigid :: VectorTransform2D tag -> Bool
isRigid transform = orthonormalSign transform == Just Positive

isOrthonormal :: VectorTransform2D tag -> Bool
isOrthonormal transform = orthonormalSign transform /= Nothing

isUniform :: VectorTransform2D tag -> Bool
isUniform transform = uniformScale transform /= Nothing

orthonormalSign :: VectorTransform2D tag -> Maybe Sign
orthonormalSign transform =
  Tolerance.using Tolerance.unitless do
    scaleFactor <- uniformScale transform
    if Number.abs scaleFactor ~= 1.0 then Just (Number.sign scaleFactor) else Nothing

uniformScale :: VectorTransform2D tag -> Maybe Number
uniformScale (VectorTransform2D vx vy) = do
  let xMagnitude = Vector2D.magnitude vx
  let yMagnitude = Vector2D.magnitude vy
  let magnitudeTolerance = Tolerance.unitless * xMagnitude
  let equalMagnitudes = Tolerance.using magnitudeTolerance (xMagnitude ~= yMagnitude)
  let orthogonalityTolerance = Tolerance.unitless * xMagnitude * xMagnitude
  let orthogonalVectors = Tolerance.using orthogonalityTolerance (vx `dot` vy ~= 0.0)
  if orthogonalVectors && equalMagnitudes then Just xMagnitude else Nothing

toRigid :: VectorTransform2D tag -> Maybe Rigid
toRigid transform = if isRigid transform then Just (coerce transform) else Nothing

toOrthonormal :: VectorTransform2D tag -> Maybe Orthonormal
toOrthonormal transform = if isOrthonormal transform then Just (coerce transform) else Nothing

toUniform :: VectorTransform2D tag -> Maybe Uniform
toUniform transform = if isUniform transform then Just (coerce transform) else Nothing

-- Helper functions to define specific/concrete transformation functions

{-# INLINE scaleInImpl #-}
scaleInImpl :: (Affine -> a -> b) -> Direction2D -> Number -> a -> b
scaleInImpl transformBy direction givenScale = transformBy (scaleIn direction givenScale)

{-# INLINE scaleAlongImpl #-}
scaleAlongImpl :: (Affine -> a -> b) -> Axis2D units -> Number -> a -> b
scaleAlongImpl transformBy axis givenScale = transformBy (scaleAlong axis givenScale)

{-# INLINE rotateByImpl #-}
rotateByImpl :: (Rigid -> a -> b) -> Angle -> a -> b
rotateByImpl transformBy angle = transformBy (rotateBy angle)

{-# INLINE mirrorInImpl #-}
mirrorInImpl :: (Orthonormal -> a -> b) -> Direction2D -> a -> b
mirrorInImpl transformBy direction = transformBy (mirrorIn direction)

{-# INLINE mirrorAcrossImpl #-}
mirrorAcrossImpl :: (Orthonormal -> a -> b) -> Axis2D units -> a -> b
mirrorAcrossImpl transformBy axis = transformBy (mirrorAcross axis)
