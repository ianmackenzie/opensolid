module OpenSolid.Csg (Operation (Union, Intersection, Difference)) where

import OpenSolid.Prelude

data Operation = Union | Intersection | Difference deriving (Eq, Show)
