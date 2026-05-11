module OpenSolid.VectorTransform (VectorTransform, VectorTransform1D (VectorTransform1D)) where

import Data.Void (Void)
import OpenSolid.Prelude
import OpenSolid.Primitives (VectorTransform2D, VectorTransform3D)

type family
  VectorTransform dimension tag space =
    transform | transform -> dimension tag space
  where
  VectorTransform 1 tag Void = VectorTransform1D tag
  VectorTransform 2 tag Void = VectorTransform2D tag
  VectorTransform 3 tag space = VectorTransform3D tag space

newtype VectorTransform1D tag = VectorTransform1D Number
