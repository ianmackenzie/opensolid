module Point3d.Type (Point3d (..)) where

import Length (Length)
import qualified Length
import OpenSolid
import qualified Show

data Point3d coordinates = Point3d !Length !Length !Length
    deriving (Eq)

instance Show (Point3d coordinates) where
    showsPrec precedence (Point3d px py pz) =
        Show.primitive precedence "Point3d.meters" [Length.inMeters px, Length.inMeters py, Length.inMeters pz]
