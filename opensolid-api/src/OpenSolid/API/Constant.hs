module OpenSolid.API.Constant
  ( Constant (Constant)
  , ffiName
  , invoke
  , documentation
  )
where

import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Constant where
  Constant :: FFI t => t -> Text -> Constant

ffiName :: FFI.ClassName -> Name -> Text
ffiName className constantName = do
  Text.join "_" ["opensolid", FFI.concatenatedName className, FFI.camelCase constantName]

invoke :: Constant -> FFI.Function
invoke (Constant value _) _ = FFI.invoke (IO.succeed value)

documentation :: Constant -> Text
documentation (Constant _ docs) = docs
