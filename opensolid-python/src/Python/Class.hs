module Python.Class (qualifiedName, unqualifiedName) where

import List qualified
import NonEmpty qualified
import OpenSolid.Prelude
import OpenSolid.FFI qualified as FFI
import OpenSolid.Text qualified as Text

qualifiedName :: FFI.Id a -> Text
qualifiedName (FFI.Id _ classNames) = do
  Text.join "." (List.map FFI.pascalCase (NonEmpty.toList classNames))

unqualifiedName :: FFI.Id a -> Text
unqualifiedName (FFI.Id _ classNames) = do
  FFI.pascalCase (NonEmpty.last classNames)
