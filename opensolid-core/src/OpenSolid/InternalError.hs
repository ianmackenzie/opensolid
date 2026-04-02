module OpenSolid.InternalError (InternalError (InternalError), throw) where

import OpenSolid.Prelude hiding (throw)
import OpenSolid.Prelude qualified as OpenSolid.Prelude

newtype InternalError = InternalError Text deriving (Show)

deriving anyclass instance Exception InternalError

throw :: Text -> a
throw message = OpenSolid.Prelude.throw (InternalError message)
