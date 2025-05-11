module API.Function (Function (..)) where

import API.ImplicitArgument (ImplicitArgument)
import Foreign qualified
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude

data Function = Function
  { ffiName :: Text
  , implicitArgument :: Maybe ImplicitArgument
  , argumentTypes :: List FFI.Type
  , returnType :: FFI.Type
  , invoke :: Foreign.Ptr () -> Foreign.Ptr () -> IO ()
  }
