module BoundingBox3d (
    BoundingBox3d (..),
    constant,
    hull2,
    hull3,
    hull4,
    aggregate,
    overlaps,
) where

import Bounds
import OpenSolid
import Point3d (Point3d (..))
import qualified Range
import Range.Unsafe

data BoundingBox3d coordinates = BoundingBox3d !(Range Meters) !(Range Meters) !(Range Meters)

instance Bounds (BoundingBox3d coordinates) where
    aggregate (BoundingBox3d x1 y1 z1) (BoundingBox3d x2 y2 z2) =
        BoundingBox3d (Range.aggregate x1 x2) (Range.aggregate y1 y2) (Range.aggregate z1 z2)

    overlaps (BoundingBox3d x1 y1 z1) (BoundingBox3d x2 y2 z2) =
        Range.overlaps x1 x2 && Range.overlaps y1 y2 && Range.overlaps z1 z2

constant :: Point3d coordinates -> BoundingBox3d coordinates
constant (Point3d x y z) =
    BoundingBox3d (Range.constant x) (Range.constant y) (Range.constant z)

hull2 :: Point3d coordinates -> Point3d coordinates -> BoundingBox3d coordinates
hull2 (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
    BoundingBox3d (Range.from x1 x2) (Range.from y1 y2) (Range.from z1 z2)

hull3 :: Point3d coordinates -> Point3d coordinates -> Point3d coordinates -> BoundingBox3d coordinates
hull3 (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) =
    let minX = min (min x1 x2) x3
        maxX = max (max x1 x2) x3
        minY = min (min y1 y2) y3
        maxY = max (max y1 y2) y3
        minZ = min (min z1 z2) z3
        maxZ = max (max z1 z2) z3
     in BoundingBox3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)

hull4 :: Point3d coordinates -> Point3d coordinates -> Point3d coordinates -> Point3d coordinates -> BoundingBox3d coordinates
hull4 (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) (Point3d x4 y4 z4) =
    let minX = min (min (min x1 x2) x3) x4
        maxX = max (max (max x1 x2) x3) x4
        minY = min (min (min y1 y2) y3) y4
        maxY = max (max (max y1 y2) y3) y4
        minZ = min (min (min z1 z2) z3) z4
        maxZ = max (max (max z1 z2) z3) z4
     in BoundingBox3d (Range minX maxX) (Range minY maxY) (Range minZ maxZ)
