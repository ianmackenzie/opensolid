module Point2d.Type (Point2d (..)) where

import OpenSolid

data Point2d coordinates = Point2d !Length !Length
    deriving (Eq, Show)
