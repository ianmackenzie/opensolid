module OpenSolid.DivisionByZero (DivisionByZero(DivisionByZero)) where

import OpenSolid.Prelude
import OpenSolid.Error qualified as Error

data DivisionByZero = DivisionByZero deriving (Eq, Show, Error.Message)
