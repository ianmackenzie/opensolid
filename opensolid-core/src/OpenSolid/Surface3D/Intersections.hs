module OpenSolid.Surface3D.Intersections (Intersections (..), intersections) where

import OpenSolid.Prelude
import OpenSolid.Surface qualified as Surface
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.Surface3D.OverlappingSegment (OverlappingSegment)

data Intersections
  = IntersectionCurves
  | OverlappingSegments (NonEmpty OverlappingSegment)

intersections ::
  Tolerance Meters =>
  Surface3D space ->
  Surface3D space ->
  Result Surface.IsDegenerate (Maybe Intersections)
intersections surface1 surface2
  | not (Surface3D.bounds surface1 `intersects` Surface3D.bounds surface2) = Ok Nothing
  | otherwise = do
      TODO
