module Python.Class
  ( qualifiedName
  , unqualifiedName
  , pointerFieldName
  )
where

import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

qualifiedName :: FFI.Class -> Text
qualifiedName = FFI.qualifiedName "."

unqualifiedName :: FFI.Class -> Text
unqualifiedName = FFI.unqualifiedName

pointerFieldName :: FFI.Class -> Text
pointerFieldName ffiClass = "_" <> Text.toLower (unqualifiedName ffiClass) <> "_ptr"
