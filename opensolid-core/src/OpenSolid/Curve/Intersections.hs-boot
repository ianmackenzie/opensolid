module OpenSolid.Curve.Intersections
  ( Intersections (..)
  , intersections
  )
where

import {-# SOURCE #-} OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Error (IsDegenerate)
import OpenSolid.Interval (Interval)
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Prelude

data Intersections dimension units space
  = IntersectionPoints (NonEmpty (IntersectionPoint dimension units space))
  | OverlappingSegments Sign (NonEmpty (Interval Unitless, Interval Unitless))

intersections ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Result IsDegenerate (Maybe (Intersections dimension units space))
