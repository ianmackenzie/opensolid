module OpenSolid.API.EqualityFunction
  ( EqualityFunction (EqualityFunction)
  , ffiName
  , invoke
  )
where

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

invoke :: EqualityFunction -> FFI.Function
invoke (EqualityFunction f) inputPtr outputPtr errorPtr = do
  (lhs, rhs) <- FFI.load inputPtr 0
  FFI.invoke (IO.succeed (f lhs rhs)) outputPtr errorPtr
