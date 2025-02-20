module API.Function (Function(..)) where

import OpenSolid.Prelude
import API.Constraint (Constraint)
import OpenSolid.FFI qualified as FFI
import Foreign qualified

data Function = Function
  { ffiName :: Text
  , constraint :: Maybe Constraint
  , argumentTypes :: List FFI.Type
  , returnType :: FFI.Type
  , invoke :: Foreign.Ptr () -> Foreign.Ptr () -> IO ()
  }
