module API.Property
  ( Property (Property)
  , ffiName
  , invoke
  , returnType
  , documentation
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Property where
  Property :: (FFI value, FFI result) => (value -> result) -> Text -> Property

ffiName :: FFI.ClassName -> Name -> Text
ffiName className propertyName =
  Text.join "_" ["opensolid", FFI.concatenatedName className, FFI.camelCase propertyName]

invoke :: Property -> Ptr () -> Ptr () -> IO ()
invoke (Property f _) inputPtr outputPtr = IO.do
  self <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f self)

returnType :: Property -> FFI.Type
returnType (Property f _) = functionReturnType f

functionReturnType :: forall value result. (FFI value, FFI result) => (value -> result) -> FFI.Type
functionReturnType _ = FFI.typeOf @result Proxy

documentation :: Property -> Text
documentation (Property _ docs) = docs
