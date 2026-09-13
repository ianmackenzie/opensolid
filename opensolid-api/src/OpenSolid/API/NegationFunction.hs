module OpenSolid.API.NegationFunction
  ( NegationFunction (NegationFunction)
  , invoke
  , ffiName
  )
where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data NegationFunction where
  NegationFunction :: FFI value => (value -> IO value) -> NegationFunction

ffiName :: FFI.ClassName -> Text
ffiName className =
  Text.join "_" ["opensolid", FFI.concatenatedName className, "neg"]

invoke :: NegationFunction -> FFI.Function
invoke (NegationFunction f) inputPtr outputPtr errorPtr = do
  value <- FFI.load inputPtr 0
  FFI.invoke (f value) outputPtr errorPtr
