module OpenSolid.Curve1d.Zeros (Error (..)) where

import Error qualified
import OpenSolid.Prelude

data Error
  = HigherOrderZero
  | ZeroEverywhere
  deriving (Eq, Show, Error.Message)
