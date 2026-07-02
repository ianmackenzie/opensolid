module OpenSolid.Curve.Intersections
  ( Intersections (..)
  )
where

import {-# SOURCE #-} OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude

data Intersections dimension units space
  = IntersectionPoints (NonEmpty (IntersectionPoint dimension units space))
  | OverlappingSegments Sign (NonEmpty (Interval Unitless, Interval Unitless))
