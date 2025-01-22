module OpenSolid.Body3d.BoundedBy (Error (..)) where

import OpenSolid.Error qualified as Error
import OpenSolid.Prelude

data Error
  = EmptyBody
  | BoundaryHasGaps
  | BoundaryIntersectsItself
  | MultipleDisjointBodies
  | BoundaryCurveHasDegeneracy
  deriving (Eq, Show, Error.Message)
