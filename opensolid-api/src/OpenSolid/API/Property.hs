module OpenSolid.API.Property
  ( Property (Property)
  , ffiName
  , invoke
  , returnType
  , documentation
  )
where

import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Property where
  Property :: (FFI value, FFI result) => (value -> IO result) -> Text -> Property

ffiName :: FFI.ClassName -> Name -> Text
ffiName className propertyName =
  Text.join "_" ["opensolid", FFI.concatenatedName className, FFI.camelCase propertyName]

invoke :: Property -> FFI.Function
invoke (Property f _) inputPtr outputPtr errorPtr = do
  self <- FFI.load inputPtr 0
  FFI.invoke (f self) outputPtr errorPtr

returnType :: Property -> FFI.Type
returnType (Property f _) = functionReturnType f

functionReturnType ::
  forall value result.
  (FFI value, FFI result) =>
  (value -> IO result) ->
  FFI.Type
functionReturnType _ = FFI.typeOf result

documentation :: Property -> Text
documentation (Property _ docs) = docs
