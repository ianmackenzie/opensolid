module Python.Class
  ( qualifiedName
  , unqualifiedName
  , pointerFieldName
  )
where

import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

qualifiedName :: FFI.ClassName -> Text
qualifiedName = FFI.qualifiedName "."

unqualifiedName :: FFI.ClassName -> Text
unqualifiedName = FFI.unqualifiedName

pointerFieldName :: FFI.ClassName -> Text
pointerFieldName className = "_" <> Text.toLower (unqualifiedName className) <> "_ptr"
