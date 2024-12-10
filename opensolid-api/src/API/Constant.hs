module API.Constant
  ( Constant (Constant)
  , ffiName
  , invoke
  , valueType
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import OpenSolid
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import Text qualified

data Constant where
  Constant :: FFI a => a -> Constant

ffiName :: FFI.Id a -> Name -> Text
ffiName classId constantName = do
  Text.join "_" ["opensolid", FFI.className classId, FFI.camelCase constantName]

invoke :: Constant -> Ptr () -> Ptr () -> IO ()
invoke (Constant value) _ outputPtr = FFI.store outputPtr 0 value

valueType :: forall a. FFI a => a -> FFI.Type
valueType _ = FFI.typeOf @a Proxy
