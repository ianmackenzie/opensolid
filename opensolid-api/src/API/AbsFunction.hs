module API.AbsFunction
  ( invoke
  , ffiName
  )
where

import Foreign (Ptr)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

ffiName :: FFI.Class -> Text
ffiName ffiClass =
  Text.join "_" ["opensolid", FFI.concatenatedName ffiClass, "abs"]

invoke :: FFI value => (value -> value) -> Ptr () -> Ptr () -> IO ()
invoke f inputPtr outputPtr = IO.do
  value <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f value)
