module OpenSolid.API.AbsFunction
  ( AbsFunction (AbsFunction)
  , invoke
  , ffiName
  )
where

import Foreign (Ptr)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data AbsFunction where
  AbsFunction :: FFI value => (value -> value) -> AbsFunction

ffiName :: FFI.ClassName -> Text
ffiName className =
  Text.join "_" ["opensolid", FFI.concatenatedName className, "abs"]

invoke :: AbsFunction -> Ptr () -> Ptr () -> IO ()
invoke (AbsFunction f) inputPtr outputPtr = do
  value <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f value)
