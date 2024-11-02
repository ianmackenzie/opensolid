module VectorCurve3d.Zeros (Error (..)) where

import Error qualified
import OpenSolid

data Error
  = HigherOrderZero
  | ZeroEverywhere
  deriving (Eq, Show, Error.Message)
