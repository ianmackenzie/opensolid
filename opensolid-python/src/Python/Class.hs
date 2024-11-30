module Python.Class (name) where

import OpenSolid
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name

name :: Name -> Maybe Name -> Text
name baseName maybeUnits = case maybeUnits of
  Nothing -> Name.pascalCase baseName
  Just units -> Name.pascalCase baseName + "_" + Name.pascalCase units
