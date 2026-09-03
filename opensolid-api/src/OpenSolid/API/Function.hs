module OpenSolid.API.Function (Function (..)) where

import Foreign qualified
import OpenSolid.API.ImplicitTolerance (ImplicitTolerance)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude

data Function = Function
  { ffiName :: Text
  , implicitTolerance :: Maybe ImplicitTolerance
  , argumentTypes :: List FFI.Type
  , returnType :: FFI.Type
  , invoke :: Foreign.Ptr () -> Foreign.Ptr () -> IO ()
  }
