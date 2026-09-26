module OpenSolid.Curve3D.IntersectionsWithSurface
  ( Intersections (..)
  , intersectionsWithSurface
  )
where

import OpenSolid.Bisection qualified as Bisection
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Curve3D.IntersectionPointWithSurface (IntersectionPointWithSurface)
import OpenSolid.Curve3D.SegmentOverlappingWithSurface (SegmentOverlappingWithSurface)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.UvBounds (UvBounds)

data Intersections space
  = IntersectionPoints (NonEmpty IntersectionPointWithSurface)
  | OverlappingSegments (NonEmpty (SegmentOverlappingWithSurface space))

type Problem space =
  ( Tolerance Meters
  , ?curve :: Curve3D space
  , ?surface :: Surface3D space
  , ?bisectionTree :: BisectionTree space
  )

type BisectionTree space =
  Bisection.Tree (Interval Unitless, UvBounds) (Curve3D.Segment space, Surface3D.Segment space)

curve :: Problem space => Curve3D space
curve = ?curve

surface :: Problem space => Surface3D space
surface = ?surface

bisectionTree :: Problem space => BisectionTree space
bisectionTree = ?bisectionTree

intersectionsWithSurface ::
  Tolerance Meters =>
  Surface3D space ->
  Curve3D space ->
  Maybe (Intersections space)
intersectionsWithSurface givenSurface givenCurve = do
  let ?curve = givenCurve
  let ?surface = givenSurface
  let curveTree = Curve.bisectionTree givenCurve
  let surfaceTree = Surface3D.bisectionTree givenSurface
  let ?bisectionTree = Bisection.pairwise curveTree surfaceTree
  findIntersections

findIntersections :: Problem space => Maybe (Intersections space)
findIntersections
  | not (Curve3D.bounds curve ^ Surface3D.bounds surface) = Nothing
  | otherwise = do
      TODO curve surface bisectionTree
