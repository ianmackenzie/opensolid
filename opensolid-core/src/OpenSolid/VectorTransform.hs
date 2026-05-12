module OpenSolid.VectorTransform
  ( VectorTransform
  , VectorTransform1D (VectorTransform1D)
  , Exists
  , toAffine
  )
where

import Data.Coerce qualified
import Data.Void (Void)
import OpenSolid.Prelude
import OpenSolid.Primitives (VectorTransform2D, VectorTransform3D)
import {-# SOURCE #-} OpenSolid.Transform qualified as Transform
import OpenSolid.VectorTransform2D qualified as VectorTransform2D
import OpenSolid.VectorTransform3D qualified as VectorTransform3D

type family
  VectorTransform dimension tag space =
    transform | transform -> dimension tag space
  where
  VectorTransform 1 tag Void = VectorTransform1D tag
  VectorTransform 2 tag Void = VectorTransform2D tag
  VectorTransform 3 tag space = VectorTransform3D tag space

newtype VectorTransform1D tag = VectorTransform1D Number

class Exists dimension space where
  toAffine ::
    VectorTransform dimension tag space ->
    VectorTransform dimension Transform.Affine space

instance Exists 1 Void where
  toAffine = Data.Coerce.coerce

instance Exists 2 Void where
  toAffine = VectorTransform2D.toAffine

instance Exists 3 space where
  toAffine = VectorTransform3D.toAffine
