module OpenSolid.API.EqualityFunction
  ( EqualityFunction (EqualityFunction)
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

data EqualityFunction where
  EqualityFunction :: FFI value => (value -> value -> Bool) -> EqualityFunction

ffiName :: FFI.ClassName -> Text
ffiName className =
  Text.join "_" ["opensolid", FFI.concatenatedName className, "eq"]

invoke :: EqualityFunction -> Ptr () -> Ptr () -> IO ()
invoke (EqualityFunction f) inputPtr outputPtr = IO.do
  (lhs, rhs) <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f lhs rhs)
