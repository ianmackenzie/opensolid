module Point3d.Type (Point3d (..)) where

import OpenSolid

data Point3d coordinates = Point3d !Length !Length !Length
    deriving (Eq, Show)
