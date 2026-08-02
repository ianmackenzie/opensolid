module OpenSolid.VectorTransform
  ( VectorTransform
  , VectorTransformExists
  , asAffine
  )
where

import OpenSolid.Primitives.Abstract (VectorTransform, VectorTransformExists)
import OpenSolid.Primitives.Abstract qualified as Primitives.Abstract
import OpenSolid.Transform.Tag qualified as Transform.Tag

asAffine ::
  VectorTransformExists dimension space =>
  VectorTransform dimension tag space ->
  VectorTransform dimension Transform.Tag.Affine space
asAffine = Primitives.Abstract.vectorTransformAsAffine
