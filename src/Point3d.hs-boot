module Point3d (Point3d (..)) where

import Bounded
import {-# SOURCE #-} BoundingBox3d (BoundingBox3d (..))
import OpenSolid

data Point3d coordinates = Point3d Length Length Length

instance Bounded (Point3d coordinates) (BoundingBox3d coordinates)
