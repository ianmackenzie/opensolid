module Python.FFI
  ( structDeclaration
  , registerType
  , dummyValue
  , argumentValue
  , outputValue
  , invoke
  )
where

import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Python qualified
import Python.Class qualified
import Python.Type.Registry (Registry)
import Python.Type.Registry qualified

typeName :: FFI.Type -> Text
typeName ffiType = case ffiType of
  FFI.Unit -> "c_int64"
  FFI.Int -> "c_int64"
  FFI.Float -> "c_double"
  FFI.Bool -> "c_int64"
  FFI.Text -> "_Text"
  FFI.List{} -> "_" <> typeNameComponent ffiType
  FFI.NonEmpty{} -> "_" <> typeNameComponent ffiType
  FFI.Array{} -> "_" <> typeNameComponent ffiType
  FFI.Tuple{} -> "_" <> typeNameComponent ffiType
  FFI.Maybe{} -> "_" <> typeNameComponent ffiType
  FFI.Result{} -> "_" <> typeNameComponent ffiType
  FFI.Class{} -> "c_void_p"

typeNameComponent :: FFI.Type -> Text
typeNameComponent ffiType = case ffiType of
  FFI.Unit -> "c_int64"
  FFI.Int -> "c_int64"
  FFI.Float -> "c_double"
  FFI.Bool -> "c_int64"
  FFI.Text -> "Text"
  FFI.List itemType -> "List_" <> typeNameComponent itemType
  FFI.NonEmpty itemType -> "List_" <> typeNameComponent itemType
  FFI.Array itemType -> "List_" <> typeNameComponent itemType
  FFI.Tuple type1 type2 rest -> do
    let itemTypes = type1 : type2 : rest
    let numItems = List.length itemTypes
    let prefix = "Tuple" <> Text.int numItems
    Text.join "_" (prefix : List.map typeNameComponent itemTypes)
  FFI.Maybe valueType -> "Maybe_" <> typeNameComponent valueType
  FFI.Result valueType -> "Result_" <> typeNameComponent valueType
  FFI.Class{} -> "c_void_p"

dummyValue :: FFI.Type -> Text
dummyValue ffiType = typeName ffiType <> "()"

dummyFieldValue :: FFI.Type -> Text
dummyFieldValue ffiType = case ffiType of
  FFI.Unit -> "0"
  FFI.Int -> "0"
  FFI.Float -> "0.0"
  FFI.Bool -> "0"
  FFI.Text -> dummyValue ffiType
  FFI.List{} -> dummyValue ffiType
  FFI.NonEmpty{} -> dummyValue ffiType
  FFI.Array{} -> dummyValue ffiType
  FFI.Tuple{} -> dummyValue ffiType
  FFI.Maybe{} -> dummyValue ffiType
  FFI.Result{} -> dummyValue ffiType
  FFI.Class{} -> dummyValue ffiType

fieldName :: Int -> Text
fieldName index = "field" <> Text.int index

structDeclaration :: Text -> List Text -> Text
structDeclaration name fieldTypes = do
  let fieldTuple index fieldType = Python.tuple [Python.str (fieldName index), fieldType]
  let fieldTuples = List.mapWithIndex fieldTuple fieldTypes
  Python.lines
    [ "class " <> name <> "(ctypes.Structure):"
    , Python.indent ["_fields_ = " <> Python.list fieldTuples]
    ]

outputValue :: FFI.Type -> Text -> Text
outputValue ffiType varName = case ffiType of
  FFI.Unit -> "None"
  FFI.Int -> varName <> ".value"
  FFI.Float -> varName <> ".value"
  FFI.Bool -> "bool(" <> varName <> ".value)"
  FFI.Text -> "_text_to_str(" <> varName <> ")"
  FFI.List itemType -> listOutputValue itemType varName
  FFI.NonEmpty itemType -> listOutputValue itemType varName
  FFI.Array itemType -> listOutputValue itemType varName
  FFI.Tuple type1 type2 rest -> tupleOutputValue varName type1 type2 rest
  FFI.Maybe valueType -> maybeOutputValue valueType varName
  FFI.Result valueType -> resultOutputValue valueType varName
  FFI.Class id -> Python.Class.qualifiedName id <> "._new(" <> varName <> ")"

fieldOutputValue :: FFI.Type -> Text -> Text
fieldOutputValue ffiType varName = case ffiType of
  FFI.Unit -> "None"
  FFI.Int -> varName
  FFI.Float -> varName
  FFI.Bool -> "bool(" <> varName <> ")"
  FFI.Text -> "_text_to_str(" <> varName <> ")"
  FFI.List itemType -> listOutputValue itemType varName
  FFI.NonEmpty itemType -> listOutputValue itemType varName
  FFI.Array itemType -> listOutputValue itemType varName
  FFI.Tuple type1 type2 rest -> tupleOutputValue varName type1 type2 rest
  FFI.Maybe valueType -> maybeOutputValue valueType varName
  FFI.Result valueType -> resultOutputValue valueType varName
  FFI.Class id -> Python.Class.qualifiedName id <> "._new(c_void_p(" <> varName <> "))"

listOutputValue :: FFI.Type -> Text -> Text
listOutputValue itemType varName =
  "[" <> fieldOutputValue itemType "item" <> " for item in [" <> varName <> ".field1[index] for index in range(" <> varName <> ".field0)]]"

tupleOutputValue :: Text -> FFI.Type -> FFI.Type -> List FFI.Type -> Text
tupleOutputValue varName type1 type2 rest = do
  let itemTypes = type1 : type2 : rest
  let itemValue index itemType = fieldOutputValue itemType (varName <> "." <> fieldName index)
  Python.tuple (List.mapWithIndex itemValue itemTypes)

maybeOutputValue :: FFI.Type -> Text -> Text
maybeOutputValue valueType varName =
  "(" <> fieldOutputValue valueType (varName <> ".field1") <> " if " <> varName <> ".field0 == 0 else None)"

resultOutputValue :: FFI.Type -> Text -> Text
resultOutputValue valueType varName = do
  let isSuccess = varName <> ".field0 == 0"
  let success = fieldOutputValue valueType (varName <> ".field2")
  let failure = "_error(_text_to_str(" <> varName <> ".field1))"
  "(" <> success <> " if " <> isSuccess <> " else " <> failure <> ")"

argumentValue :: List (Text, FFI.Type) -> Text
argumentValue [] = "c_void_p()"
argumentValue [(var1, type1)] = singleArgument var1 type1
argumentValue arguments@((_, type1) : (_, type2) : rest) = do
  let tupleType = FFI.Tuple type1 type2 (List.map Pair.second rest)
  let constructor = typeName tupleType
  let constructorArgument (varName, varType) = fieldArgumentValue varName varType
  Python.call constructor (List.map constructorArgument arguments)

singleArgument :: Text -> FFI.Type -> Text
singleArgument varName ffiType = case ffiType of
  FFI.Unit -> "c_int64()"
  FFI.Int -> "c_int64(" <> varName <> ")"
  FFI.Float -> "c_double(" <> varName <> ")"
  FFI.Bool -> "c_int64(" <> varName <> ")"
  FFI.Text -> "_str_to_text(" <> varName <> ")"
  FFI.List itemType -> listArgumentValue ffiType itemType varName
  FFI.NonEmpty itemType -> nonEmptyArgumentValue itemType varName
  FFI.Array itemType -> nonEmptyArgumentValue itemType varName
  FFI.Tuple type1 type2 rest -> tupleArgumentValue ffiType type1 type2 rest varName
  FFI.Maybe valueType -> maybeArgumentValue ffiType valueType varName
  FFI.Result{} -> internalError "Should never have Result as input argument"
  FFI.Class{} -> varName <> "._ptr"

fieldArgumentValue :: Text -> FFI.Type -> Text
fieldArgumentValue varName ffiType = case ffiType of
  FFI.Unit -> "0"
  FFI.Int -> varName
  FFI.Float -> varName
  FFI.Bool -> varName
  FFI.Text -> "_str_to_text(" <> varName <> ")"
  FFI.List itemType -> listArgumentValue ffiType itemType varName
  FFI.NonEmpty itemType -> nonEmptyArgumentValue itemType varName
  FFI.Array itemType -> nonEmptyArgumentValue itemType varName
  FFI.Tuple type1 type2 rest -> tupleArgumentValue ffiType type1 type2 rest varName
  FFI.Maybe valueType -> maybeArgumentValue ffiType valueType varName
  FFI.Result{} -> internalError "Should never have Result as input argument"
  FFI.Class{} -> varName <> "._ptr"

listArgumentValue :: FFI.Type -> FFI.Type -> Text -> Text
listArgumentValue listType itemType varName = do
  let arrayType = "(" <> typeName itemType <> " * len(" <> varName <> "))"
  let arrayItems = "[" <> singleArgument "item" itemType <> " for item in " <> varName <> "]"
  let array = arrayType <> "(*" <> arrayItems <> ")"
  "_list_argument(" <> typeName listType <> "," <> array <> ")"

nonEmptyArgumentValue :: FFI.Type -> Text -> Text
nonEmptyArgumentValue itemType varName = do
  let listValue = listArgumentValue (FFI.List itemType) itemType varName
  "(" <> listValue <> " if " <> varName <> " else _error('List is empty'))"

tupleArgumentValue :: FFI.Type -> FFI.Type -> FFI.Type -> List FFI.Type -> Text -> Text
tupleArgumentValue tupleType type1 type2 rest varName = do
  let itemTypes = type1 : type2 : rest
  let itemValue index itemType = fieldArgumentValue (varName <> "[" <> Text.int index <> "]") itemType
  Python.call (typeName tupleType) (List.mapWithIndex itemValue itemTypes)

maybeArgumentValue :: FFI.Type -> FFI.Type -> Text -> Text
maybeArgumentValue maybeType valueType varName = do
  let constructor = typeName maybeType
  let justExpression = Python.call constructor ["0", fieldArgumentValue varName valueType]
  let nothingExpression = Python.call constructor ["1", dummyFieldValue valueType]
  "(" <> justExpression <> " if " <> varName <> " is not None else " <> nothingExpression <> ")"

registerType :: FFI.Type -> Registry -> Registry
registerType ffiType registry = do
  let name = typeName ffiType
  if Python.Type.Registry.contains name registry
    then registry
    else case ffiType of
      FFI.Unit -> registry
      FFI.Int -> registry
      FFI.Float -> registry
      FFI.Bool -> registry
      FFI.Text -> registry
      FFI.List itemType -> registerList ffiType itemType registry
      FFI.NonEmpty itemType -> registerList (FFI.List itemType) itemType registry
      FFI.Array itemType -> registerList (FFI.List itemType) itemType registry
      FFI.Tuple type1 type2 rest -> registerTuple ffiType type1 type2 rest registry
      FFI.Maybe valueType -> registerMaybe ffiType valueType registry
      FFI.Result valueType -> registerResult ffiType valueType registry
      FFI.Class{} -> registry

registerList :: FFI.Type -> FFI.Type -> Registry -> Registry
registerList listType itemType registry = do
  let listTypeName = typeName listType
  let declaration =
        structDeclaration listTypeName ["c_int64", Python.call "POINTER" [typeName itemType]]
  Python.Type.Registry.add listTypeName declaration registry

registerTuple :: FFI.Type -> FFI.Type -> FFI.Type -> List FFI.Type -> Registry -> Registry
registerTuple tupleType type1 type2 rest registry = do
  let tupleTypeName = typeName tupleType
  let itemTypes = type1 : type2 : rest
  let declaration = structDeclaration tupleTypeName (List.map typeName itemTypes)
  List.foldr registerType registry itemTypes
    |> Python.Type.Registry.add tupleTypeName declaration

registerMaybe :: FFI.Type -> FFI.Type -> Registry -> Registry
registerMaybe maybeType valueType registry = do
  let maybeTypeName = typeName maybeType
  let declaration = structDeclaration maybeTypeName ["c_int64", typeName valueType]
  registry
    |> registerType valueType
    |> Python.Type.Registry.add maybeTypeName declaration

registerResult :: FFI.Type -> FFI.Type -> Registry -> Registry
registerResult resultType valueType registry = do
  let resultTypeName = typeName resultType
  let declaration = structDeclaration resultTypeName ["c_int64", "_Text", typeName valueType]
  registry
    |> registerType valueType
    |> Python.Type.Registry.add resultTypeName declaration

invoke :: Text -> Text -> Text -> Text
invoke ffiFunctionName inputPtr outputPtr =
  Python.call ("_lib." <> ffiFunctionName) [inputPtr, outputPtr]
