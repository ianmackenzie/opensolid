module OpenSolid.Region2D.BoundedBy (Error (..)) where

import OpenSolid.Prelude

data Error
  = EmptyRegion
  | BoundaryHasGaps
  | BoundaryIntersectsItself
  | MultipleDisjointRegions
  | BoundaryCurveHasDegeneracy
  deriving (Eq, Show)
