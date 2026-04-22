module OpenSolid.Region2D.BoundedBy (Error (..)) where

import OpenSolid.Prelude

data Error
  = EmptyRegion
  | BoundaryHasGaps
  | BoundaryIntersectsItself
  | MultipleDisjointRegions
  deriving (Eq, Show)
