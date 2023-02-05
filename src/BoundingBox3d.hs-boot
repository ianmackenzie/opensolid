module BoundingBox3d (BoundingBox3d (..)) where

import Bounds
import OpenSolid
import Range (Range)

type role BoundingBox3d nominal nominal

type BoundingBox3d :: Type -> Type -> Type
data BoundingBox3d units coordinates = BoundingBox3d (Range units) (Range units) (Range units)

instance Bounds (BoundingBox3d units coordinates)
