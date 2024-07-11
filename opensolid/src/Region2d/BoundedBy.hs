module Region2d.BoundedBy (Error (..)) where

import Error qualified
import OpenSolid

data Error
  = EmptyRegion
  | BoundaryHasGaps
  | BoundaryIntersectsItself
  | MultipleDisjointRegions
  | BoundaryCurveHasDegeneracy
  | BoundaryCurvesHaveHigherOrderIntersection
  deriving (Eq, Show, Error.Message)
