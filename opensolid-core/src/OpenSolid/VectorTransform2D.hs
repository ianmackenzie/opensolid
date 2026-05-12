module OpenSolid.VectorTransform2D
  ( VectorTransform2D
  , Rigid
  , Orthonormal
  , Uniform
  , Affine
  , identity
  , coerce
  , handedness
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
import {-# SOURCE #-} OpenSolid.Transform qualified as Transform

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

coerce :: VectorTransform2D tag1 -> VectorTransform2D tag2
coerce = Data.Coerce.coerce

handedness :: VectorTransform2D tag -> Sign
handedness (VectorTransform2D vx vy) = Number.sign (vx `cross` vy)

scaleBy :: Number -> Uniform
scaleBy scale = VectorTransform2D (Vector2D scale 0.0) (Vector2D 0.0 scale)

scaleIn :: Direction2D -> Number -> Affine
scaleIn (Direction2D dx dy) scale = do
  let dx2 = dx * dx
  let dy2 = dy * dy
  let xy = (scale - 1.0) * dx * dy
  let vx = Vector2D (scale * dx2 + dy2) xy
  let vy = Vector2D xy (scale * dy2 + dx2)
  VectorTransform2D vx vy

scaleAlong :: Axis2D units -> Number -> Affine
scaleAlong (Axis2D _ direction) scale = scaleIn direction scale

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

{-# INLINE scaleInImpl #-}
scaleInImpl :: (Affine -> a -> b) -> Direction2D -> Number -> a -> b
scaleInImpl transformBy direction scale = transformBy (scaleIn direction scale)

{-# INLINE scaleAlongImpl #-}
scaleAlongImpl :: (Affine -> a -> b) -> Axis2D units -> Number -> a -> b
scaleAlongImpl transformBy axis scale = transformBy (scaleAlong axis scale)

{-# INLINE rotateByImpl #-}
rotateByImpl :: (Rigid -> a -> b) -> Angle -> a -> b
rotateByImpl transformBy angle = transformBy (rotateBy angle)

{-# INLINE mirrorInImpl #-}
mirrorInImpl :: (Orthonormal -> a -> b) -> Direction2D -> a -> b
mirrorInImpl transformBy direction = transformBy (mirrorIn direction)

{-# INLINE mirrorAcrossImpl #-}
mirrorAcrossImpl :: (Orthonormal -> a -> b) -> Axis2D units -> a -> b
mirrorAcrossImpl transformBy axis = transformBy (mirrorAcross axis)
