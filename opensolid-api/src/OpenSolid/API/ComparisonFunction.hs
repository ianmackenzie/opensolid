module OpenSolid.API.ComparisonFunction
  ( ComparisonFunction (ComparisonFunction)
  , ffiName
  , invoke
  )
where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data ComparisonFunction where
  ComparisonFunction :: FFI value => (value -> value -> Int) -> ComparisonFunction

ffiName :: FFI.ClassName -> Text
ffiName className =
  Text.join "_" ["opensolid", FFI.concatenatedName className, "compare"]

invoke :: ComparisonFunction -> FFI.Function
invoke (ComparisonFunction f) inputPtr outputPtr errorPtr = do
  (lhs, rhs) <- FFI.load inputPtr 0
  FFI.invoke (IO.succeed (f lhs rhs)) outputPtr errorPtr
