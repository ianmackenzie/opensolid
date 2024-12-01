module Python.Class (qualifiedName, unqualifiedName) where

import List qualified
import NonEmpty qualified
import OpenSolid
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI qualified as FFI
import Text qualified

qualifiedName :: FFI.Id a -> Text
qualifiedName (FFI.Id _ classNames maybeUnits) = do
  let joinedClassNames = Text.join "." (List.map Name.pascalCase (NonEmpty.toList classNames))
  case maybeUnits of
    Nothing -> joinedClassNames
    Just units -> joinedClassNames + "_" + Name.pascalCase units

unqualifiedName :: FFI.Id a -> Text
unqualifiedName (FFI.Id _ classNames maybeUnits) = do
  let className = Name.pascalCase (NonEmpty.last classNames)
  case maybeUnits of
    Nothing -> className
    Just units -> className + "_" + Name.pascalCase units
