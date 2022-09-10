module VectorBoundingBox3d (
    VectorBoundingBox3d (..),
    singleton,
    hull2,
    hull3,
    hull4,
) where

import Interval (Interval)
import qualified Interval
import Interval.Unsafe
import OpenSolid
import Vector3d (Vector3d (..))

data VectorBoundingBox3d units coordinates = VectorBoundingBox3d (Interval units) (Interval units) (Interval units)

singleton :: Vector3d units coordinates -> VectorBoundingBox3d units coordinates
singleton point =
    let (Vector3d x y z) = point
        xInterval = Interval.singleton x
        yInterval = Interval.singleton y
        zInterval = Interval.singleton z
     in VectorBoundingBox3d xInterval yInterval zInterval

hull2 :: Vector3d units coordinates -> Vector3d units coordinates -> VectorBoundingBox3d units coordinates
hull2 p1 p2 =
    let (Vector3d x1 y1 z1) = p1
        (Vector3d x2 y2 z2) = p2
        xInterval = Interval.from x1 x2
        yInterval = Interval.from y1 y2
        zInterval = Interval.from z1 z2
     in VectorBoundingBox3d xInterval yInterval zInterval

hull3 :: Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> VectorBoundingBox3d units coordinates
hull3 p1 p2 p3 =
    let (Vector3d x1 y1 z1) = p1
        (Vector3d x2 y2 z2) = p2
        (Vector3d x3 y3 z3) = p3
        minX = min (min x1 x2) x3
        maxX = max (max x1 x2) x3
        minY = min (min y1 y2) y3
        maxY = max (max y1 y2) y3
        minZ = min (min z1 z2) z3
        maxZ = max (max z1 z2) z3
        xInterval = Interval minX maxX
        yInterval = Interval minY maxY
        zInterval = Interval minZ maxZ
     in VectorBoundingBox3d xInterval yInterval zInterval

hull4 :: Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> VectorBoundingBox3d units coordinates
hull4 p1 p2 p3 p4 =
    let (Vector3d x1 y1 z1) = p1
        (Vector3d x2 y2 z2) = p2
        (Vector3d x3 y3 z3) = p3
        (Vector3d x4 y4 z4) = p4
        minX = min (min (min x1 x2) x3) x4
        maxX = max (max (max x1 x2) x3) x4
        minY = min (min (min y1 y2) y3) y4
        maxY = max (max (max y1 y2) y3) y4
        minZ = min (min (min z1 z2) z3) z4
        maxZ = max (max (max z1 z2) z3) z4
        xInterval = Interval minX maxX
        yInterval = Interval minY maxY
        zInterval = Interval minZ maxZ
     in VectorBoundingBox3d xInterval yInterval zInterval
