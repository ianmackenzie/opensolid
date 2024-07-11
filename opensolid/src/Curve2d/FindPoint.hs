module Curve2d.FindPoint (Error (..)) where

import Error qualified
import OpenSolid

data Error
  = CurveIsCoincidentWithPoint
  | HigherOrderSolution
  deriving (Eq, Show, Error.Message)
