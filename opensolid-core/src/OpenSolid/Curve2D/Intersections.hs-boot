module OpenSolid.Curve2D.Intersections
  ( Intersections (..)
  , intersections
  )
where

import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve2D.OverlappingSegment (OverlappingSegment)
import OpenSolid.Prelude

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments (NonEmpty OverlappingSegment)

intersections ::
  Tolerance units =>
  Curve2D units space ->
  Curve2D units space ->
  Result Curve.IsPoint (Maybe Intersections)
