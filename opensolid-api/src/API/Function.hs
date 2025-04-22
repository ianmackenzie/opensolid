module API.Function (Function (..)) where

import API.Constraint (Constraint)
import Foreign qualified
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude

data Function = Function
  { ffiName :: Text
  , constraint :: Maybe Constraint
  , argumentTypes :: List FFI.Type
  , returnType :: FFI.Type
  , invoke :: Foreign.Ptr () -> Foreign.Ptr () -> IO ()
  }
