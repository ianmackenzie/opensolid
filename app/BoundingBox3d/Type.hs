module BoundingBox3d.Type (BoundingBox3d (..)) where

import Bounds
import OpenSolid
import Range (Range)
import qualified Range

data BoundingBox3d coordinates = BoundingBox3d !(Range Length) !(Range Length) !(Range Length)

instance Bounds (BoundingBox3d coordinates) where
    aggregate (BoundingBox3d x1 y1 z1) (BoundingBox3d x2 y2 z2) =
        BoundingBox3d (Range.aggregate x1 x2) (Range.aggregate y1 y2) (Range.aggregate z1 z2)

    overlaps (BoundingBox3d x1 y1 z1) (BoundingBox3d x2 y2 z2) =
        Range.overlaps x1 x2 && Range.overlaps y1 y2 && Range.overlaps z1 z2
