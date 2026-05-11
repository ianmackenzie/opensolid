module OpenSolid.VectorTransform2D
  ( VectorTransform2D (VectorTransform2D)
  , Rigid
  , Orthonormal
  , Uniform
  , Affine
  , identity
  , coerce
  , handedness
  , rotateBy
  , mirrorAcross
  , scaleBy
  , scaleIn
  , placeIn
  , relativeTo
  , toOrthonormal
  , toUniform
  , toAffine
  , rotateByImpl
  , mirrorAcrossImpl
  , scaleByImpl
  , scaleInImpl
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction2D (Direction2D)
  , Frame2D
  , Vector2D (Vector2D)
  , VectorTransform2D (VectorTransform2D)
  )
import OpenSolid.Transform qualified as Transform
import OpenSolid.Vector2D qualified as Vector2D

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

rotateBy :: Angle -> Rigid
rotateBy angle = do
  let cos = Angle.cos angle
  let sin = Angle.sin angle
  let vx = Vector2D cos sin
  let vy = Vector2D -sin cos
  VectorTransform2D vx vy

mirrorAcross :: Direction2D -> Orthonormal
mirrorAcross (Direction2D dx dy) = do
  let vx = Vector2D (1.0 - 2.0 * dy * dy) (2.0 * dx * dy)
  let vy = Vector2D (2.0 * dx * dy) (1.0 - 2.0 * dx * dx)
  VectorTransform2D vx vy

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

placeIn :: Frame2D units -> VectorTransform2D tag -> VectorTransform2D tag
placeIn frame transform = do
  let vx =
        unitX
          & Vector2D.relativeTo frame
          & (* transform)
          & Vector2D.placeIn frame
  let vy =
        unitY
          & Vector2D.relativeTo frame
          & (* transform)
          & Vector2D.placeIn frame
  VectorTransform2D vx vy

relativeTo :: Frame2D units -> VectorTransform2D tag -> VectorTransform2D tag
relativeTo frame transform = do
  let vx =
        unitX
          & Vector2D.placeIn frame
          & (* transform)
          & Vector2D.relativeTo frame
  let vy =
        unitY
          & Vector2D.placeIn frame
          & (* transform)
          & Vector2D.relativeTo frame
  VectorTransform2D vx vy

toOrthonormal :: Transform.IsOrthonormal tag => VectorTransform2D tag -> Orthonormal
toOrthonormal = Data.Coerce.coerce

toUniform :: Transform.IsUniform tag => VectorTransform2D tag -> Uniform
toUniform = Data.Coerce.coerce

toAffine :: VectorTransform2D tag -> Affine
toAffine = Data.Coerce.coerce

rotateByImpl :: (Rigid -> a -> b) -> Angle -> a -> b
rotateByImpl transformBy angle = transformBy (rotateBy angle)

mirrorAcrossImpl :: (Orthonormal -> a -> b) -> Direction2D -> a -> b
mirrorAcrossImpl transformBy direction = transformBy (mirrorAcross direction)

scaleByImpl :: (Uniform -> a -> b) -> Number -> a -> b
scaleByImpl transformBy scale = transformBy (scaleBy scale)

scaleInImpl :: (Affine -> a -> b) -> Direction2D -> Number -> a -> b
scaleInImpl transformBy direction scale = transformBy (scaleIn direction scale)
