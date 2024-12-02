module Python.FFI
  ( structDeclaration
  , registerType
  , dummyValue
  , argumentValue
  , outputValue
  , invoke
  )
where

import List qualified
import OpenSolid
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Python qualified
import Python.Class qualified
import Python.Type.Registry (Registry)
import Python.Type.Registry qualified
import Text qualified

typeName :: FFI.Type -> Text
typeName ffiType = case ffiType of
  FFI.Int -> "c_int64"
  FFI.Float -> "c_double"
  FFI.Qty _ -> "c_double"
  FFI.List{} -> "_" + typeNameComponent ffiType
  FFI.Tuple{} -> "_" + typeNameComponent ffiType
  FFI.Maybe{} -> "_" + typeNameComponent ffiType
  FFI.Result{} -> "_" + typeNameComponent ffiType
  FFI.Class{} -> "c_void_p"

typeNameComponent :: FFI.Type -> Text
typeNameComponent ffiType = case ffiType of
  FFI.Int -> "c_int64"
  FFI.Float -> "c_double"
  FFI.Qty{} -> "c_double"
  FFI.List itemType -> "List_" + typeNameComponent itemType
  FFI.Tuple type1 type2 rest -> do
    let itemTypes = type1 : type2 : rest
    let numItems = List.length itemTypes
    let prefix = "Tuple" + Text.int numItems
    Text.join "_" (prefix : List.map typeNameComponent itemTypes)
  FFI.Maybe valueType -> "Maybe_" + typeNameComponent valueType
  FFI.Result valueType -> "Result_" + typeNameComponent valueType
  FFI.Class{} -> "c_void_p"

dummyValue :: FFI.Type -> Text
dummyValue ffiType = typeName ffiType + "()"

dummyFieldValue :: FFI.Type -> Text
dummyFieldValue ffiType = case ffiType of
  FFI.Int -> "0"
  FFI.Float -> "0.0"
  FFI.Qty{} -> "0.0"
  FFI.List{} -> dummyValue ffiType
  FFI.Tuple{} -> dummyValue ffiType
  FFI.Maybe{} -> dummyValue ffiType
  FFI.Result{} -> dummyValue ffiType
  FFI.Class{} -> dummyValue ffiType

fieldName :: Int -> Text
fieldName index = "field" + Text.int index

structDeclaration :: Text -> List Text -> Text
structDeclaration name fieldTypes = do
  let fieldTuple index fieldType = Python.tuple [Python.str (fieldName index), fieldType]
  let fieldTuples = List.mapWithIndex fieldTuple fieldTypes
  Python.lines
    [ "class " + name + "(Structure):"
    , Python.indent ["_fields_ = " + Python.list fieldTuples]
    ]

outputValue :: FFI.Type -> Text -> Text
outputValue ffiType varName = case ffiType of
  FFI.Int -> varName + ".value"
  FFI.Float -> varName + ".value"
  FFI.Qty className -> Python.call (Name.pascalCase className) [varName + ".value"]
  FFI.List itemType -> listOutputValue itemType varName
  FFI.Tuple type1 type2 rest -> tupleOutputValue varName type1 type2 rest
  FFI.Maybe valueType -> maybeOutputValue valueType varName
  FFI.Result valueType -> resultOutputValue valueType varName
  FFI.Class id -> Python.Class.qualifiedName id + "(ptr = " + varName + ")"

fieldOutputValue :: FFI.Type -> Text -> Text
fieldOutputValue ffiType varName = case ffiType of
  FFI.Int -> varName
  FFI.Float -> varName
  FFI.Qty className -> Name.pascalCase className + "(" + varName + ")"
  FFI.List itemType -> listOutputValue itemType varName
  FFI.Tuple type1 type2 rest -> tupleOutputValue varName type1 type2 rest
  FFI.Maybe valueType -> maybeOutputValue valueType varName
  FFI.Result valueType -> resultOutputValue valueType varName
  FFI.Class id -> Python.Class.qualifiedName id + "(ptr=c_void_p(" + varName + "))"

listOutputValue :: FFI.Type -> Text -> Text
listOutputValue itemType varName =
  "[" + fieldOutputValue itemType "item" + " for item in [" + varName + ".field1[index] for index in range(" + varName + ".field0)]]"

tupleOutputValue :: Text -> FFI.Type -> FFI.Type -> List FFI.Type -> Text
tupleOutputValue varName type1 type2 rest = do
  let itemTypes = type1 : type2 : rest
  let itemValue index itemType = fieldOutputValue itemType (varName + "." + fieldName index)
  Python.tuple (List.mapWithIndex itemValue itemTypes)

maybeOutputValue :: FFI.Type -> Text -> Text
maybeOutputValue valueType varName =
  "(" + fieldOutputValue valueType (varName + ".field1") + " if " + varName + ".field0 == 0 else None)"

resultOutputValue :: FFI.Type -> Text -> Text
resultOutputValue valueType varName =
  "(" + fieldOutputValue valueType (varName + ".field2") + " if " + varName + ".field0 == 0 else _error(" + varName + "))"

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
  FFI.Int -> Python.call "c_int64" [varName]
  FFI.Float -> Python.call "c_double" [varName]
  FFI.Qty{} -> Python.call "c_double" [varName + ".value"]
  FFI.List{} -> TODO
  FFI.Tuple type1 type2 rest -> tupleArgumentValue ffiType type1 type2 rest varName
  FFI.Maybe valueType -> maybeArgumentValue ffiType valueType varName
  FFI.Result{} -> internalError "Should never have Result as input argument"
  FFI.Class{} -> varName + ".__ptr__"

fieldArgumentValue :: Text -> FFI.Type -> Text
fieldArgumentValue varName ffiType = case ffiType of
  FFI.Int -> varName
  FFI.Float -> varName
  FFI.Qty{} -> varName + ".value"
  FFI.List{} -> TODO
  FFI.Tuple type1 type2 rest -> tupleArgumentValue ffiType type1 type2 rest varName
  FFI.Maybe valueType -> maybeArgumentValue ffiType valueType varName
  FFI.Result{} -> internalError "Should never have Result as input argument"
  FFI.Class{} -> varName + ".__ptr__"

tupleArgumentValue :: FFI.Type -> FFI.Type -> FFI.Type -> List FFI.Type -> Text -> Text
tupleArgumentValue tupleType type1 type2 rest varName = do
  let itemTypes = type1 : type2 : rest
  let itemValue index itemType = fieldArgumentValue (varName + "[" + Text.int index + "]") itemType
  Python.call (typeName tupleType) (List.mapWithIndex itemValue itemTypes)

maybeArgumentValue :: FFI.Type -> FFI.Type -> Text -> Text
maybeArgumentValue maybeType valueType varName = do
  let constructor = typeName maybeType
  let justExpression = Python.call constructor ["0", fieldArgumentValue varName valueType]
  let nothingExpression = Python.call constructor ["1", dummyFieldValue valueType]
  "(" + justExpression + " if " + varName + " is not None else " + nothingExpression + ")"

registerType :: FFI.Type -> Registry -> Registry
registerType ffiType registry = do
  let name = typeName ffiType
  if Python.Type.Registry.contains name registry
    then registry
    else case ffiType of
      FFI.Int -> registry
      FFI.Float -> registry
      FFI.Qty _ -> registry
      FFI.List itemType -> registerList ffiType itemType registry
      FFI.Tuple type1 type2 rest -> registerTuple ffiType type1 type2 rest registry
      FFI.Maybe valueType -> registerMaybe ffiType valueType registry
      FFI.Result valueType -> registerResult ffiType valueType registry
      FFI.Class{} -> registry

registerList :: FFI.Type -> FFI.Type -> Registry -> Registry
registerList listType itemType registry = do
  let listTypeName = typeName listType
  let declaration = structDeclaration listTypeName ["c_int64", Python.call "POINTER" [typeName itemType]]
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
  let declaration =
        structDeclaration resultTypeName ["c_int64", "_ErrorMessage", typeName valueType]
  registry
    |> registerType valueType
    |> Python.Type.Registry.add resultTypeName declaration

invoke :: Text -> Text -> Text -> Text
invoke ffiFunctionName inputPtr outputPtr =
  Python.call ("_lib." + ffiFunctionName) [inputPtr, outputPtr]