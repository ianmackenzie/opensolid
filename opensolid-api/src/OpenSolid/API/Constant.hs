module OpenSolid.API.Constant
  ( Constant (Constant)
  , ffiName
  , invoke
  , documentation
  )
where

import Foreign (Ptr)
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Constant where
  Constant :: FFI t => t -> Text -> Constant

ffiName :: FFI.ClassName -> Name -> Text
ffiName className constantName = do
  Text.join "_" ["opensolid", FFI.concatenatedName className, FFI.camelCase constantName]

invoke :: Constant -> Ptr () -> Ptr () -> IO ()
invoke (Constant value _) _ outputPtr = FFI.store outputPtr 0 value

documentation :: Constant -> Text
documentation (Constant _ docs) = docs
