module OpenSolid.API.Argument (Kind (Positional, Named), kind) where

import Data.Proxy (Proxy)
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude hiding (Named)
import OpenSolid.Text qualified as Text

data Kind = Positional | Named deriving (Eq, Ord)

kind :: FFI a => Name -> Proxy a -> Kind
kind name proxy =
  case FFI.argumentName proxy of
    Nothing -> Positional
    Just argName ->
      -- Slight hack: use conversion to snake case as a way to do case-insensitive comparison,
      -- so that e.g. a Haskell "verticalFov" argument is allowed to be exposed as
      -- "Vertical FOV" instead of "Vertical Fov"
      if FFI.snakeCase name == FFI.snakeCase argName
        then Named
        else do
          let message = "Argument name mismatch: " <> Text.show name <> " /= " <> Text.show argName
          internalError message
