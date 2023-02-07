module Transformable
  ( IsTransformable
  , IsScalable
  , IsDeformable
  , Transformable
  , Scalable
  , Deformable
  )
where

class IsTransformable a

class IsTransformable a => IsScalable a

class IsScalable a => IsDeformable a

data Transformable

instance IsTransformable Transformable

data Scalable

instance IsTransformable Scalable

instance IsScalable Scalable

data Deformable

instance IsTransformable Deformable

instance IsScalable Deformable

instance IsDeformable Deformable
