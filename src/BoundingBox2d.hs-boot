module BoundingBox2d (BoundingBox2d (..)) where

import Bounds
import OpenSolid
import Range (Range)

data BoundingBox2d coordinates = BoundingBox2d (Range Meters) (Range Meters)

instance Bounds (BoundingBox2d coordinates)
