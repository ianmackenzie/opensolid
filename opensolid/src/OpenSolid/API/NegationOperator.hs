module OpenSolid.API.NegationOperator
  ( invoke
  , ffiName
  )
where

import Foreign (Ptr)
import IO qualified
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Text qualified

ffiName :: FFI.Id a -> Text
ffiName classId =
  Text.join "_" ["opensolid", FFI.className classId, "neg"]

invoke :: FFI value => (value -> value) -> Ptr () -> Ptr () -> IO ()
invoke f inputPtr outputPtr = IO.do
  value <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f value)
