module OpenSolid.Curve.Nondegenerate.Intersections (intersections) where

import {-# SOURCE #-} OpenSolid.Curve (Curve, CurveExists)
import OpenSolid.Curve.Intersections (Intersections)
import OpenSolid.NewtonRaphson.Surface qualified as NewtonRaphson.Surface
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Prelude

intersections ::
  ( CurveExists dimension units space
  , NewtonRaphson.Surface.Solver dimension units space
  , Tolerance units
  ) =>
  Nondegenerate (Curve dimension units space) ->
  Nondegenerate (Curve dimension units space) ->
  Maybe Intersections
