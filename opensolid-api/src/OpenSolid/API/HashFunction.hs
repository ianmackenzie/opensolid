module OpenSolid.API.HashFunction
  ( HashFunction (HashFunction)
  , ffiName
  , invoke
  )
where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data HashFunction where
  HashFunction :: FFI value => (value -> Int) -> HashFunction

ffiName :: FFI.ClassName -> Text
ffiName className =
  Text.join "_" ["opensolid", FFI.concatenatedName className, "hash"]

invoke :: HashFunction -> FFI.Function
invoke (HashFunction f) inputPtr outputPtr errorPtr = do
  value <- FFI.load inputPtr 0
  FFI.invoke (IO.succeed (f value)) outputPtr errorPtr
