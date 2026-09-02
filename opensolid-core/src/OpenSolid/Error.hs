module OpenSolid.Error
  ( IsZero (IsZero)
  , IsDegenerate (IsDegenerate)
  )
where

import OpenSolid.Prelude

data IsDegenerate = IsDegenerate deriving (Eq, Show, Err)

data IsZero = IsZero deriving (Eq, Show, Err)
