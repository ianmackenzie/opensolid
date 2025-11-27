module OpenSolid.Curve2d.Intersections
  ( Error
  , Intersections (..)
  , intersections
  )
where

import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve2d.OverlappingSegment (OverlappingSegment)
import OpenSolid.Prelude

data Error

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments (NonEmpty OverlappingSegment)

intersections ::
  Tolerance units =>
  Curve2d units space ->
  Curve2d units space ->
  Result Error (Maybe Intersections)
