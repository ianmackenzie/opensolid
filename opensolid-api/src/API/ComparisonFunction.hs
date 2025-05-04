module API.ComparisonFunction
  ( invoke
  , ffiName
  , valueType
  , returnType
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

ffiName :: FFI.Class -> Text
ffiName ffiClass =
  Text.join "_" ["opensolid", FFI.concatenatedName ffiClass, "compare"]

invoke :: FFI value => (value -> value -> Int) -> Ptr () -> Ptr () -> IO ()
invoke f inputPtr outputPtr = IO.do
  (lhs, rhs) <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f lhs rhs)

valueType :: forall value. FFI value => (value -> value -> Int) -> FFI.Type
valueType _ = FFI.typeOf @value Proxy

returnType :: FFI.Type
returnType = FFI.typeOf @Int Proxy
