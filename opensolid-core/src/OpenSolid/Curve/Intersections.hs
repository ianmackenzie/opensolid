module OpenSolid.Curve.Intersections
  ( Intersections (..)
  )
where

import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments
      Sign
      (NonEmpty (Interval Unitless, Interval Unitless))
      (List IntersectionPoint)
  deriving (Show)
