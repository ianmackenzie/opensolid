module API.Constant
  ( Constant (Constant)
  , ffiName
  , invoke
  , valueType
  , documentation
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import OpenSolid.Prelude
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Text qualified as Text

data Constant where
  Constant :: FFI a => a -> Text -> Constant

ffiName :: FFI.Id a -> Name -> Text
ffiName classId constantName = do
  Text.join "_" ["opensolid", FFI.className classId, FFI.camelCase constantName]

invoke :: Constant -> Ptr () -> Ptr () -> IO ()
invoke (Constant value _) _ outputPtr = FFI.store outputPtr 0 value

valueType :: forall a. FFI a => a -> FFI.Type
valueType _ = FFI.typeOf @a Proxy

documentation :: Constant -> Text
documentation (Constant _ docs) = docs
