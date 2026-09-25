module OpenSolid.Curve.Intersections (Intersections (..), intersections) where

import {-# SOURCE #-} OpenSolid.Curve (Curve, CurveExists)
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Interval (Interval)
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Prelude

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments
      Sign
      (NonEmpty (Interval Unitless, Interval Unitless))
      (List IntersectionPoint)

intersections ::
  ( CurveExists dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Maybe Intersections
