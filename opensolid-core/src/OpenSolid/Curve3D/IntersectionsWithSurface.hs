module OpenSolid.Curve3D.IntersectionsWithSurface
  ( Intersections (..)
  , intersectionsWithSurface
  )
where

import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Curve3D.IntersectionPointWithSurface (IntersectionPointWithSurface)
import OpenSolid.Curve3D.SegmentOverlappingWithSurface (SegmentOverlappingWithSurface)
import OpenSolid.Prelude
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D

data Intersections space
  = IntersectionPoints (NonEmpty (IntersectionPointWithSurface space))
  | OverlappingSegments (NonEmpty (SegmentOverlappingWithSurface space))

type Problem space =
  ( Tolerance Meters
  , ?curve :: Curve3D space
  , ?surface :: Surface3D space
  )

curve :: Problem space => Curve3D space
curve = ?curve

surface :: Problem space => Surface3D space
surface = ?surface

searchTree :: Problem space => (Curve3D.Segment space, Surface3D.Segment space)
searchTree = (Curve.searchTree curve, Surface3D.searchTree surface)

intersectionsWithSurface ::
  Tolerance Meters =>
  Surface3D space ->
  Curve3D space ->
  Maybe (Intersections space)
intersectionsWithSurface givenSurface givenCurve = do
  let ?curve = givenCurve
  let ?surface = givenSurface
  findIntersections

findIntersections :: Problem space => Maybe (Intersections space)
findIntersections
  | not (Curve3D.bounds curve ^ Surface3D.bounds surface) = Nothing
  | otherwise = do
      TODO curve surface searchTree
