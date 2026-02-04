module OpenSolid.Curve.Intersections
  ( Intersections (..)
  , intersections
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve.OverlappingSegment (OverlappingSegment)
import OpenSolid.Prelude

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments (NonEmpty OverlappingSegment)
  deriving (Show)

intersections ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Result Curve.IsPoint (Maybe Intersections)
intersections curve1 curve2
  | not (Curve.bounds curve1 `intersects` Curve.bounds curve2) = Ok Nothing
  | otherwise = do
      TODO
