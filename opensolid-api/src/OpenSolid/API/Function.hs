module OpenSolid.API.Function (Function (..)) where

import OpenSolid.API.ImplicitTolerance (ImplicitTolerance)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude

data Function = Function
  { ffiName :: Text
  , implicitTolerance :: Maybe ImplicitTolerance
  , argumentTypes :: List FFI.Type
  , returnType :: FFI.Type
  , invoke :: FFI.Function
  }
