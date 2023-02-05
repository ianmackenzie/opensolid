module BoundingBox2d (BoundingBox2d (..)) where

import Bounds
import OpenSolid
import Range (Range)

type role BoundingBox2d nominal nominal

type BoundingBox2d :: Type -> Type -> Type
data BoundingBox2d coordinates units = BoundingBox2d (Range units) (Range units)

instance Bounds (BoundingBox2d coordinates units)
