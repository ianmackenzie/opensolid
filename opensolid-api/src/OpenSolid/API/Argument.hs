module OpenSolid.API.Argument (Kind (Positional, Named), kind) where

import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Kind = Positional | Named deriving (Eq, Ord, Show)

kind :: forall t -> FFI t => Name -> Kind
kind t name =
  case FFI.argumentName t of
    Nothing -> Positional
    Just argName ->
      -- Slight hack: use conversion to snake case as a way to do case-insensitive comparison,
      -- so that e.g. a Haskell "verticalFov" argument is allowed to be exposed as
      -- "Vertical FOV" instead of "Vertical Fov"
      if FFI.snakeCase name == FFI.snakeCase argName
        then Named
        else do
          let message = "Argument name mismatch: " <> Text.show name <> " /= " <> Text.show argName
          throw (InternalError message)
