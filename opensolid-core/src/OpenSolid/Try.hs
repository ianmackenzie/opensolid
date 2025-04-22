module OpenSolid.Try ((>>=), (>>)) where

import OpenSolid.Composition qualified as Composition
import OpenSolid.Debug (Debug)
import OpenSolid.Error qualified as Error
import OpenSolid.Result (Result (Failure, Success))
import OpenSolid.Result qualified as Result
import OpenSolid.Text (Text)

(>>=) :: Result x a -> (a -> Result y b) -> Result Text b
Success value >>= function = Result.try (function value)
Failure error >>= _ = Failure (Error.message error)

(>>) :: Debug -> Result x a -> Result Text a
debug >> result = Result.try ((Composition.>>) debug result)
