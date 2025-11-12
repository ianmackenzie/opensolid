module OpenSolid.Body3d.BoundedBy (Error (..)) where

import OpenSolid.Prelude

data Error
  = EmptyBody
  | BoundaryHasGaps
  | BoundaryIntersectsItself
  | BoundaryCurveHasDegeneracy
  deriving (Eq, Show)
