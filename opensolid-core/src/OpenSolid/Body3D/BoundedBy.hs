module OpenSolid.Body3D.BoundedBy (Error (..)) where

import OpenSolid.Prelude

data Error
  = EmptyBody
  | BoundaryHasGaps
  | BoundaryIntersectsItself
  deriving (Eq, Show)
