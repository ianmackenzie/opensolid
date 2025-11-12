module OpenSolid.InternalError (InternalError (InternalError)) where

import OpenSolid.Prelude

newtype InternalError = InternalError Text deriving (Show)

deriving anyclass instance Exception InternalError
