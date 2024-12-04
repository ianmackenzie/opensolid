module OpenSolid.API.EqualityFunction
  ( invoke
  , ffiName
  , valueType
  , returnType
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
  Text.join "_" ["opensolid", FFI.className classId, "eq"]

invoke :: FFI value => (value -> value -> Bool) -> Ptr () -> Ptr () -> IO ()
invoke f inputPtr outputPtr = IO.do
  (lhs, rhs) <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f lhs rhs)

valueType :: forall value. FFI value => (value -> value -> Bool) -> FFI.Type
valueType _ = FFI.typeOf @value Proxy

returnType :: FFI.Type
returnType = FFI.typeOf @Bool Proxy
