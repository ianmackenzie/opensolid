module OpenSolid.API.ComparisonFunction
  ( ComparisonFunction (ComparisonFunction)
  , ffiName
  , invoke
  )
where

import Foreign (Ptr)
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

invoke :: ComparisonFunction -> Ptr () -> Ptr () -> IO ()
invoke (ComparisonFunction f) inputPtr outputPtr = IO.do
  (lhs, rhs) <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f lhs rhs)
