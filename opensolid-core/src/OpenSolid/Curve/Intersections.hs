module OpenSolid.Curve.Intersections
  ( Intersections (..)
  , intersections
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve.Nondegenerate.Intersections qualified as Curve.Nondegenerate.Intersections
import OpenSolid.Error (IsDegenerate)
import OpenSolid.Interval (Interval)
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments Sign (NonEmpty (Interval Unitless, Interval Unitless))
  deriving (Show)

intersections ::
  ( Curve.Exists dimension units space
  , NewtonRaphson.Surface dimension units space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Result IsDegenerate (Maybe Intersections)
intersections curve1 curve2 =
  Result.map2
    Curve.Nondegenerate.Intersections.intersections
    (Curve.nondegenerate curve1)
    (Curve.nondegenerate curve2)
