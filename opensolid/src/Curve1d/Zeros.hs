module Curve1d.Zeros (Error (..)) where

import Error qualified
import OpenSolid

data Error
  = HigherOrderZero
  | ZeroEverywhere
  deriving (Eq, Show, Error.Message)
