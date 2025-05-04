module Python.Class
  ( qualifiedName
  , unqualifiedName
  , pointerFieldName
  )
where

import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude

qualifiedName :: FFI.Class -> Text
qualifiedName = FFI.qualifiedName "."

unqualifiedName :: FFI.Class -> Text
unqualifiedName = FFI.unqualifiedName

pointerFieldName :: FFI.Class -> Text
pointerFieldName ffiClass = "_" <> unqualifiedName ffiClass <> "_ptr"
