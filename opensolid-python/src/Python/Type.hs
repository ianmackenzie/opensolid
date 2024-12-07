module Python.Type (qualifiedName) where

import List qualified
import OpenSolid
import OpenSolid.FFI qualified as FFI
import Python.Class qualified
import Text qualified

qualifiedName :: FFI.Type -> Text
qualifiedName ffiType = case ffiType of
  FFI.Int -> "int"
  FFI.Float -> "float"
  FFI.Bool -> "bool"
  FFI.Text -> "str"
  FFI.List itemType -> "list[" + qualifiedName itemType + "]"
  FFI.NonEmpty itemType -> "list[" + qualifiedName itemType + "]"
  FFI.Tuple first second rest -> do
    let itemTypeNames = List.map qualifiedName (first : second : rest)
    "tuple[" + Text.join "," itemTypeNames + "]"
  FFI.Maybe valueType -> qualifiedName valueType + " | None"
  FFI.Result valueType -> qualifiedName valueType
  FFI.Class id -> Python.Class.qualifiedName id
