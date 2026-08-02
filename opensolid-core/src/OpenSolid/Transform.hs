module OpenSolid.Transform
  ( Transform
  , TransformExists
  , vectorTransform
  , asAffine
  , uniformScale
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives.Abstract (Transform, TransformExists, VectorTransform)
import OpenSolid.Primitives.Abstract qualified as Primitives.Abstract
import OpenSolid.Transform.Tag qualified as Transform.Tag

vectorTransform ::
  TransformExists dimension units space =>
  Transform dimension tag units space ->
  VectorTransform dimension tag space
vectorTransform = Primitives.Abstract.transformVectorTransform

asAffine ::
  TransformExists dimension units space =>
  Transform dimension tag units space ->
  Transform dimension Transform.Tag.Affine units space
asAffine = Primitives.Abstract.transformAsAffine

uniformScale ::
  TransformExists dimension units space =>
  Transform dimension tag units space ->
  Maybe Number
uniformScale = Primitives.Abstract.transformUniformScale
