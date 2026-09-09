module OpenSolid.IsDegenerate (IsDegenerate (IsDegenerate)) where

import OpenSolid.Prelude

data IsDegenerate a = IsDegenerate a deriving (Eq, Show, Err)
