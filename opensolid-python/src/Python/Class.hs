module Python.Class
  ( qualifiedName
  , unqualifiedName
  , pointerFieldName
  )
where

import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

qualifiedName :: FFI.Id a -> Text
qualifiedName (FFI.Id _ classNames) = do
  Text.join "." (List.map FFI.pascalCase (NonEmpty.toList classNames))

unqualifiedName :: FFI.Id a -> Text
unqualifiedName (FFI.Id _ classNames) = do
  FFI.pascalCase (NonEmpty.last classNames)

pointerFieldName :: FFI.Id a -> Text
pointerFieldName id = "_" <> unqualifiedName id <> "_ptr"
