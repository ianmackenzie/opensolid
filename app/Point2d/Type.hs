module Point2d.Type (Point2d (..)) where

import Length (Length)
import qualified Length
import OpenSolid
import qualified Show

data Point2d coordinates = Point2d !Length !Length
    deriving (Eq)

instance Show (Point2d coordinates) where
    showsPrec precedence (Point2d px py) =
        Show.primitive precedence "Point2d.meters" [Length.inMeters px, Length.inMeters py]
