module API.NegationFunction
  ( NegationFunction (NegationFunction)
  , invoke
  , ffiName
  )
where

import Foreign (Ptr)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data NegationFunction where
  NegationFunction :: FFI value => (value -> value) -> NegationFunction

ffiName :: FFI.ClassName -> Text
ffiName className =
  Text.join "_" ["opensolid", FFI.concatenatedName className, "neg"]

invoke :: NegationFunction -> Ptr () -> Ptr () -> IO ()
invoke (NegationFunction f) inputPtr outputPtr = IO.do
  value <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f value)
