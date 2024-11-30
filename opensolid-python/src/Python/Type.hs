module Python.Type (name) where

import List qualified
import OpenSolid
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI qualified as FFI
import Python.Class qualified
import Text qualified

name :: FFI.Type -> Text
name ffiType = case ffiType of
  FFI.Int -> "int"
  FFI.Float -> "float"
  FFI.Qty qtyName -> Name.pascalCase qtyName
  FFI.List itemType -> "list[" + name itemType + "]"
  FFI.Tuple first second rest -> do
    let itemTypeNames = List.map name (first : second : rest)
    "tuple[" + Text.join "," itemTypeNames + "]"
  FFI.Maybe valueType -> name valueType + " | None"
  FFI.Result valueType -> name valueType
  FFI.Class baseName maybeUnits -> Python.Class.name baseName maybeUnits
