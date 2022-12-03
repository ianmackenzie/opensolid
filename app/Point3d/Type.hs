module Point3d.Type (Point3d (..)) where

import Bounded
import {-# SOURCE #-} BoundingBox3d (BoundingBox3d (..))
import OpenSolid
import qualified Range

data Point3d coordinates = Point3d !Length !Length !Length
    deriving (Eq, Show)

instance Bounded (Point3d coordinates) (BoundingBox3d coordinates) where
    bounds (Point3d x y z) = BoundingBox3d (Range.constant x) (Range.constant y) (Range.constant z)
