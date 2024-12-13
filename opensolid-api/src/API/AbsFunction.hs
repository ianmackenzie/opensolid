module API.AbsFunction
  ( invoke
  , ffiName
  , valueType
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import IO qualified
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Text qualified

ffiName :: FFI.Id a -> Text
ffiName classId =
  Text.join "_" ["opensolid", FFI.className classId, "abs"]

invoke :: FFI value => (value -> value) -> Ptr () -> Ptr () -> IO ()
invoke f inputPtr outputPtr = IO.do
  value <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f value)

valueType :: forall value. FFI value => (value -> value) -> FFI.Type
valueType _ = FFI.typeOf @value Proxy
