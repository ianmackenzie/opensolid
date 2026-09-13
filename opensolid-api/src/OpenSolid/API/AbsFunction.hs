module OpenSolid.API.AbsFunction
  ( AbsFunction (AbsFunction)
  , invoke
  , ffiName
  )
where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data AbsFunction where
  AbsFunction :: FFI value => (value -> IO value) -> AbsFunction

ffiName :: FFI.ClassName -> Text
ffiName className =
  Text.join "_" ["opensolid", FFI.concatenatedName className, "abs"]

invoke :: AbsFunction -> FFI.Function
invoke (AbsFunction f) inputPtr outputPtr errorPtr = do
  value <- FFI.load inputPtr 0
  FFI.invoke (f value) outputPtr errorPtr
