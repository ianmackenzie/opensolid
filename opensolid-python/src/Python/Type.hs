module Python.Type (qualifiedName) where

import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Python.Class qualified

qualifiedName :: FFI.Type -> Text
qualifiedName ffiType = case ffiType of
  FFI.Unit -> "None"
  FFI.Int -> "int"
  FFI.Float -> "float"
  FFI.Bool -> "bool"
  FFI.Sign -> "int"
  FFI.Text -> "str"
  FFI.List itemType -> "list[" <> qualifiedName itemType <> "]"
  FFI.NonEmpty itemType -> "list[" <> qualifiedName itemType <> "]"
  FFI.Array itemType -> "list[" <> qualifiedName itemType <> "]"
  FFI.Tuple first second rest -> do
    let itemTypeNames = List.map qualifiedName (first : second : rest)
    "tuple[" <> Text.join "," itemTypeNames <> "]"
  FFI.Maybe valueType -> qualifiedName valueType <> " | None"
  FFI.Result valueType -> qualifiedName valueType
  FFI.Class classId -> Python.Class.qualifiedName classId
