module OpenSolid.Curve3D.IntersectionsWithSurface
  ( Intersections (..)
  , Error (..)
  , intersectionsWithSurface
  )
where

import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Curve3D.IntersectionPointWithSurface (IntersectionPointWithSurface)
import OpenSolid.Curve3D.SegmentOverlappingWithSurface (SegmentOverlappingWithSurface)
import OpenSolid.Prelude
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D

data Intersections space
  = IntersectionPoints (NonEmpty IntersectionPointWithSurface)
  | OverlappingSegments (NonEmpty (SegmentOverlappingWithSurface space))

data Error
  = CurveIsPoint
  | SurfaceIsPoint

intersectionsWithSurface ::
  Tolerance Meters =>
  Surface3D space ->
  Curve3D space ->
  Result Error (Maybe (Intersections space))
intersectionsWithSurface surface curve
  | not (Curve3D.bounds curve `intersects` Surface3D.bounds surface) = Ok Nothing
  | otherwise = do
      TODO
