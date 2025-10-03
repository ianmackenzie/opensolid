module OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero)) where

import OpenSolid.Error qualified as Error
import OpenSolid.Prelude

data DivisionByZero = DivisionByZero deriving (Eq, Show, Error.Message)
