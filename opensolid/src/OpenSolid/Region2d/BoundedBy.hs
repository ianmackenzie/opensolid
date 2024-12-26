module OpenSolid.Region2d.BoundedBy (Error (..)) where

import Error qualified
import OpenSolid.Prelude

data Error
  = EmptyRegion
  | BoundaryHasGaps
  | BoundaryIntersectsItself
  | MultipleDisjointRegions
  | BoundaryCurveHasDegeneracy
  | BoundaryCurvesHaveHigherOrderIntersection
  deriving (Eq, Show, Error.Message)
