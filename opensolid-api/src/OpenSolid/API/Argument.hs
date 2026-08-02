module OpenSolid.API.Argument (Kind (Positional, Named), kind) where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude

data Kind = Positional | Named deriving (Eq, Ord, Show)

kind :: forall t -> FFI t => Kind
kind t = if FFI.isNamedArgument t then Named else Positional
