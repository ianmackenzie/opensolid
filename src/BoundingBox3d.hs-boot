module BoundingBox3d (BoundingBox3d (..)) where

import Bounds
import OpenSolid
import Range (Range)

data BoundingBox3d coordinates = BoundingBox3d (Range Meters) (Range Meters) (Range Meters)

instance Bounds (BoundingBox3d coordinates)
