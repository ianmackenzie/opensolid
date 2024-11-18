module CTypes
  ( structDeclaration
  , registerType
  , typeName
  , dummyValue
  , argumentValue1
  , argumentValue2
  , argumentValue3
  , argumentValue4
  , argumentValue5
  , argumentValue6
  , outputValue
  , invoke
  )
where

import Data.Proxy (Proxy (Proxy))
import List qualified
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Python qualified
import Text qualified
import TypeRegistry (TypeRegistry)
import TypeRegistry qualified

typeName :: forall a. FFI a => Proxy a -> Text
typeName proxy = case FFI.representation proxy of
  FFI.Int -> "c_int64"
  FFI.Float -> "c_double"
  FFI.Qty _ -> "c_double"
  FFI.List -> "_" + simpleTypeName proxy
  FFI.Tuple2 -> "_" + simpleTypeName proxy
  FFI.Tuple3 -> "_" + simpleTypeName proxy
  FFI.Tuple4 -> "_" + simpleTypeName proxy
  FFI.Tuple5 -> "_" + simpleTypeName proxy
  FFI.Tuple6 -> "_" + simpleTypeName proxy
  FFI.Maybe -> "_" + simpleTypeName proxy
  FFI.Result -> "_" + simpleTypeName proxy
  FFI.Class _ -> "c_void_p"

simpleTypeName :: forall a. FFI a => Proxy a -> Text
simpleTypeName proxy = case FFI.representation proxy of
  FFI.Int -> "c_int64"
  FFI.Float -> "c_double"
  FFI.Qty _ -> "c_double"
  FFI.List -> "List"
  FFI.Tuple2 -> tuple2TypeName proxy
  FFI.Tuple3 -> tuple3TypeName proxy
  FFI.Tuple4 -> tuple4TypeName proxy
  FFI.Tuple5 -> tuple5TypeName proxy
  FFI.Tuple6 -> tuple6TypeName proxy
  FFI.Maybe -> maybeTypeName proxy
  FFI.Result -> resultTypeName proxy
  FFI.Class _ -> "c_void_p"

compositeTypeName :: List Text -> Text
compositeTypeName = Text.join "_"

tuple2TypeName :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Text
tuple2TypeName _ =
  compositeTypeName
    [ "Tuple2"
    , simpleTypeName @a Proxy
    , simpleTypeName @b Proxy
    ]

tuple3TypeName :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> Text
tuple3TypeName _ =
  compositeTypeName
    [ "Tuple3"
    , simpleTypeName @a Proxy
    , simpleTypeName @b Proxy
    , simpleTypeName @c Proxy
    ]

tuple4TypeName :: forall a b c d. (FFI a, FFI b, FFI c, FFI d) => Proxy (a, b, c, d) -> Text
tuple4TypeName _ =
  compositeTypeName
    [ "Tuple4"
    , simpleTypeName @a Proxy
    , simpleTypeName @b Proxy
    , simpleTypeName @c Proxy
    , simpleTypeName @d Proxy
    ]

tuple5TypeName :: forall a b c d e. (FFI a, FFI b, FFI c, FFI d, FFI e) => Proxy (a, b, c, d, e) -> Text
tuple5TypeName _ =
  compositeTypeName
    [ "Tuple5"
    , simpleTypeName @a Proxy
    , simpleTypeName @b Proxy
    , simpleTypeName @c Proxy
    , simpleTypeName @d Proxy
    , simpleTypeName @e Proxy
    ]

tuple6TypeName :: forall a b c d e f. (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) => Proxy (a, b, c, d, e, f) -> Text
tuple6TypeName _ =
  compositeTypeName
    [ "Tuple6"
    , simpleTypeName @a Proxy
    , simpleTypeName @b Proxy
    , simpleTypeName @c Proxy
    , simpleTypeName @d Proxy
    , simpleTypeName @e Proxy
    , simpleTypeName @f Proxy
    ]

maybeTypeName :: forall a. FFI a => Proxy (Maybe a) -> Text
maybeTypeName _ = compositeTypeName ["Maybe", simpleTypeName @a Proxy]

resultTypeName :: forall x a. FFI a => Proxy (Result x a) -> Text
resultTypeName _ = compositeTypeName ["Result", simpleTypeName @a Proxy]

dummyValue :: forall a. FFI a => Proxy a -> Text
dummyValue proxy = typeName proxy + "()"

-- case FFI.representation proxy of
--  FFI.Int -> "c_int64()"
--  FFI.Float -> "c_double()"
--  FFI.Qty _ -> "c_double()"
--  FFI.List -> dummyList proxy
--  FFI.Tuple2 -> dummyTuple2 proxy
--  FFI.Tuple3 -> dummyTuple3 proxy
--  FFI.Tuple4 -> dummyTuple4 proxy
--  FFI.Tuple5 -> dummyTuple5 proxy
--  FFI.Tuple6 -> dummyTuple6 proxy
--  FFI.Maybe -> dummyMaybe proxy
--  FFI.Result -> dummyResult proxy
--  FFI.Class _ -> "c_void_p()"

dummyFieldValue :: forall a. FFI a => Proxy a -> Text
dummyFieldValue proxy = case FFI.representation proxy of
  FFI.Int -> "0"
  FFI.Float -> "0.0"
  FFI.Qty _ -> "0.0"
  FFI.List -> dummyValue proxy
  FFI.Tuple2 -> dummyValue proxy
  FFI.Tuple3 -> dummyValue proxy
  FFI.Tuple4 -> dummyValue proxy
  FFI.Tuple5 -> dummyValue proxy
  FFI.Tuple6 -> dummyValue proxy
  FFI.Maybe -> dummyValue proxy
  FFI.Result -> dummyValue proxy
  FFI.Class _ -> dummyValue proxy

-- dummyList :: forall a. FFI a => Proxy (List a) -> Text
-- dummyList proxy =
--   Python.call (typeName proxy) $
--     [ "c_int64()"
--     , "c_void_p()"
--     ]

-- dummyTuple2 :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Text
-- dummyTuple2 proxy =
--   Python.call (typeName proxy) $
--     [ dummyFieldValue @a Proxy
--     , dummyFieldValue @b Proxy
--     ]

-- dummyTuple3 :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> Text
-- dummyTuple3 proxy =
--   Python.call (typeName proxy) $
--     [ dummyFieldValue @a Proxy
--     , dummyFieldValue @b Proxy
--     , dummyFieldValue @c Proxy
--     ]

-- dummyTuple4 :: forall a b c d. (FFI a, FFI b, FFI c, FFI d) => Proxy (a, b, c, d) -> Text
-- dummyTuple4 proxy =
--   Python.call (typeName proxy) $
--     [ dummyFieldValue @a Proxy
--     , dummyFieldValue @b Proxy
--     , dummyFieldValue @c Proxy
--     , dummyFieldValue @d Proxy
--     ]

-- dummyTuple5 ::
--   forall a b c d e.
--   (FFI a, FFI b, FFI c, FFI d, FFI e) =>
--   Proxy (a, b, c, d, e) ->
--   Text
-- dummyTuple5 proxy =
--   Python.call (typeName proxy) $
--     [ dummyFieldValue @a Proxy
--     , dummyFieldValue @b Proxy
--     , dummyFieldValue @c Proxy
--     , dummyFieldValue @d Proxy
--     , dummyFieldValue @e Proxy
--     ]

-- dummyTuple6 ::
--   forall a b c d e f.
--   (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
--   Proxy (a, b, c, d, e, f) ->
--   Text
-- dummyTuple6 proxy =
--   Python.call (typeName proxy) $
--     [ dummyFieldValue @a Proxy
--     , dummyFieldValue @b Proxy
--     , dummyFieldValue @c Proxy
--     , dummyFieldValue @d Proxy
--     , dummyFieldValue @e Proxy
--     , dummyFieldValue @f Proxy
--     ]

-- dummyMaybe :: forall a. FFI a => Proxy (Maybe a) -> Text
-- dummyMaybe proxy = typeName proxy + "()"

-- dummyResult :: forall x a. FFI a => Proxy (Result x a) -> Text
-- dummyResult proxy = typeName proxy + "()"

fieldName :: Int -> Text
fieldName index = "field" + Text.int index

structDeclaration :: Text -> List Text -> Text
structDeclaration name fieldTypes = do
  let fieldTuple index fieldType = Python.tuple [Python.str (fieldName index), fieldType]
  let fieldTuples = List.mapWithIndex fieldTuple fieldTypes
  Python.lines
    [ "class " + name + "(Structure):"
    , Python.indent ["_fields_ = " + Python.list fieldTuples + " # noqa: RUF012"]
    ]

outputValue :: forall a. FFI a => Proxy a -> Text -> Text
outputValue proxy varName = case FFI.representation proxy of
  FFI.Int -> varName + ".value"
  FFI.Float -> varName + ".value"
  FFI.Qty className -> Python.call className [varName + ".value"]
  FFI.List -> TODO
  FFI.Tuple2 -> tuple2OutputValue proxy varName
  FFI.Tuple3 -> tuple3OutputValue proxy varName
  FFI.Tuple4 -> tuple4OutputValue proxy varName
  FFI.Tuple5 -> tuple5OutputValue proxy varName
  FFI.Tuple6 -> tuple6OutputValue proxy varName
  FFI.Maybe -> maybeOutputValue proxy varName
  FFI.Result -> resultOutputValue proxy varName
  FFI.Class className -> className + "(__ptr__ = " + varName + ")"

fieldOutputValue :: forall a. FFI a => Proxy a -> Text -> Text
fieldOutputValue proxy varName = case FFI.representation proxy of
  FFI.Int -> varName
  FFI.Float -> varName
  FFI.Qty className -> className + "(" + varName + ")"
  FFI.List -> TODO
  FFI.Tuple2 -> tuple2OutputValue proxy varName
  FFI.Tuple3 -> tuple3OutputValue proxy varName
  FFI.Tuple4 -> tuple4OutputValue proxy varName
  FFI.Tuple5 -> tuple5OutputValue proxy varName
  FFI.Tuple6 -> tuple6OutputValue proxy varName
  FFI.Maybe -> maybeOutputValue proxy varName
  FFI.Result -> resultOutputValue proxy varName
  FFI.Class className -> className + "(__ptr__=c_void_p(" + varName + "))"

tuple2OutputValue :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Text -> Text
tuple2OutputValue _ varName =
  Python.tuple
    [ fieldOutputValue @a Proxy (varName + ".field0")
    , fieldOutputValue @b Proxy (varName + ".field1")
    ]

tuple3OutputValue :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> Text -> Text
tuple3OutputValue _ varName =
  Python.tuple
    [ fieldOutputValue @a Proxy (varName + ".field0")
    , fieldOutputValue @b Proxy (varName + ".field1")
    , fieldOutputValue @c Proxy (varName + ".field2")
    ]

tuple4OutputValue ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Proxy (a, b, c, d) ->
  Text ->
  Text
tuple4OutputValue _ varName =
  Python.tuple
    [ fieldOutputValue @a Proxy (varName + ".field0")
    , fieldOutputValue @b Proxy (varName + ".field1")
    , fieldOutputValue @c Proxy (varName + ".field2")
    , fieldOutputValue @d Proxy (varName + ".field3")
    ]

tuple5OutputValue ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Proxy (a, b, c, d, e) ->
  Text ->
  Text
tuple5OutputValue _ varName =
  Python.tuple
    [ fieldOutputValue @a Proxy (varName + ".field0")
    , fieldOutputValue @b Proxy (varName + ".field1")
    , fieldOutputValue @c Proxy (varName + ".field2")
    , fieldOutputValue @d Proxy (varName + ".field3")
    , fieldOutputValue @e Proxy (varName + ".field4")
    ]

tuple6OutputValue ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Proxy (a, b, c, d, e, f) ->
  Text ->
  Text
tuple6OutputValue _ varName =
  Python.tuple
    [ fieldOutputValue @a Proxy (varName + ".field0")
    , fieldOutputValue @b Proxy (varName + ".field1")
    , fieldOutputValue @c Proxy (varName + ".field2")
    , fieldOutputValue @d Proxy (varName + ".field3")
    , fieldOutputValue @e Proxy (varName + ".field4")
    , fieldOutputValue @f Proxy (varName + ".field5")
    ]

maybeOutputValue :: forall a. FFI a => Proxy (Maybe a) -> Text -> Text
maybeOutputValue _ varName =
  "(" + fieldOutputValue @a Proxy (varName + ".field1") + " if " + varName + ".field0 == 0 else None)"

resultOutputValue :: forall x a. FFI a => Proxy (Result x a) -> Text -> Text
resultOutputValue _ varName =
  "(" + fieldOutputValue @a Proxy (varName + ".field2") + " if " + varName + ".field0 == 0 else _error(" + varName + "))"

argumentValue1 :: forall a. FFI a => Proxy a -> Text -> Text
argumentValue1 proxy varName = case FFI.representation proxy of
  FFI.Int -> Python.call "c_int64" [varName]
  FFI.Float -> Python.call "c_double" [varName]
  FFI.Qty _ -> Python.call "c_double" [varName + ".value"]
  FFI.List -> TODO
  FFI.Tuple2 -> tuple2ArgumentValue proxy varName
  FFI.Tuple3 -> tuple3ArgumentValue proxy varName
  FFI.Tuple4 -> tuple4ArgumentValue proxy varName
  FFI.Tuple5 -> tuple5ArgumentValue proxy varName
  FFI.Tuple6 -> tuple6ArgumentValue proxy varName
  FFI.Maybe -> maybeArgumentValue proxy varName
  FFI.Result -> internalError "Should never have Result as input argument"
  FFI.Class _ -> varName + ".__ptr__"

fieldArgumentValue :: forall a. FFI a => Proxy a -> Text -> Text
fieldArgumentValue proxy varName = case FFI.representation proxy of
  FFI.Int -> varName
  FFI.Float -> varName
  FFI.Qty _ -> varName + ".value"
  FFI.List -> TODO
  FFI.Tuple2 -> tuple2ArgumentValue proxy varName
  FFI.Tuple3 -> tuple3ArgumentValue proxy varName
  FFI.Tuple4 -> tuple4ArgumentValue proxy varName
  FFI.Tuple5 -> tuple5ArgumentValue proxy varName
  FFI.Tuple6 -> tuple6ArgumentValue proxy varName
  FFI.Maybe -> maybeArgumentValue proxy varName
  FFI.Result -> internalError "Should never have Result as input argument"
  FFI.Class _ -> varName + ".__ptr__"

tuple2ArgumentValue :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Text -> Text
tuple2ArgumentValue proxy varName =
  Python.call (typeName proxy) $
    [ fieldArgumentValue @a Proxy (varName + "[0]")
    , fieldArgumentValue @b Proxy (varName + "[1]")
    ]

tuple3ArgumentValue :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> Text -> Text
tuple3ArgumentValue proxy varName =
  Python.call (typeName proxy) $
    [ fieldArgumentValue @a Proxy (varName + "[0]")
    , fieldArgumentValue @b Proxy (varName + "[1]")
    , fieldArgumentValue @c Proxy (varName + "[2]")
    ]

tuple4ArgumentValue ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Proxy (a, b, c, d) ->
  Text ->
  Text
tuple4ArgumentValue proxy varName =
  Python.call (typeName proxy) $
    [ fieldArgumentValue @a Proxy (varName + "[0]")
    , fieldArgumentValue @b Proxy (varName + "[1]")
    , fieldArgumentValue @c Proxy (varName + "[2]")
    , fieldArgumentValue @d Proxy (varName + "[3]")
    ]

tuple5ArgumentValue ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Proxy (a, b, c, d, e) ->
  Text ->
  Text
tuple5ArgumentValue proxy varName =
  Python.call (typeName proxy) $
    [ fieldArgumentValue @a Proxy (varName + "[0]")
    , fieldArgumentValue @b Proxy (varName + "[1]")
    , fieldArgumentValue @c Proxy (varName + "[2]")
    , fieldArgumentValue @d Proxy (varName + "[3]")
    , fieldArgumentValue @e Proxy (varName + "[4]")
    ]

tuple6ArgumentValue ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Proxy (a, b, c, d, e, f) ->
  Text ->
  Text
tuple6ArgumentValue proxy varName =
  Python.call (typeName proxy) $
    [ fieldArgumentValue @a Proxy (varName + "[0]")
    , fieldArgumentValue @b Proxy (varName + "[1]")
    , fieldArgumentValue @c Proxy (varName + "[2]")
    , fieldArgumentValue @d Proxy (varName + "[3]")
    , fieldArgumentValue @e Proxy (varName + "[4]")
    , fieldArgumentValue @f Proxy (varName + "[5]")
    ]

maybeArgumentValue :: forall a. FFI a => Proxy (Maybe a) -> Text -> Text
maybeArgumentValue proxy varName =
  "("
    + Python.call (typeName proxy) ["0", fieldArgumentValue @a Proxy varName]
    + " if "
    + varName
    + " is not None else "
    + Python.call (typeName proxy) ["1", dummyFieldValue @a Proxy]
    + ")"

argumentValue2 :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Text -> Text -> Text
argumentValue2 proxy varName1 varName2 =
  Python.call (typeName proxy) $
    [ fieldArgumentValue @a Proxy varName1
    , fieldArgumentValue @b Proxy varName2
    ]

argumentValue3 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Proxy (a, b, c) ->
  Text ->
  Text ->
  Text ->
  Text
argumentValue3 proxy varName1 varName2 varName3 =
  Python.call (typeName proxy) $
    [ fieldArgumentValue @a Proxy varName1
    , fieldArgumentValue @b Proxy varName2
    , fieldArgumentValue @c Proxy varName3
    ]

argumentValue4 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Proxy (a, b, c, d) ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text
argumentValue4 proxy varName1 varName2 varName3 varName4 =
  Python.call (typeName proxy) $
    [ fieldArgumentValue @a Proxy varName1
    , fieldArgumentValue @b Proxy varName2
    , fieldArgumentValue @c Proxy varName3
    , fieldArgumentValue @d Proxy varName4
    ]

argumentValue5 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Proxy (a, b, c, d, e) ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text
argumentValue5 proxy varName1 varName2 varName3 varName4 varName5 =
  Python.call (typeName proxy) $
    [ fieldArgumentValue @a Proxy varName1
    , fieldArgumentValue @b Proxy varName2
    , fieldArgumentValue @c Proxy varName3
    , fieldArgumentValue @d Proxy varName4
    , fieldArgumentValue @e Proxy varName5
    ]

argumentValue6 ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Proxy (a, b, c, d, e, f) ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text
argumentValue6 proxy varName1 varName2 varName3 varName4 varName5 varName6 =
  Python.call (typeName proxy) $
    [ fieldArgumentValue @a Proxy varName1
    , fieldArgumentValue @b Proxy varName2
    , fieldArgumentValue @c Proxy varName3
    , fieldArgumentValue @d Proxy varName4
    , fieldArgumentValue @e Proxy varName5
    , fieldArgumentValue @f Proxy varName6
    ]

registerType :: forall a. FFI a => Proxy a -> TypeRegistry -> TypeRegistry
registerType proxy registry = do
  let name = typeName proxy
  if TypeRegistry.contains name registry
    then registry
    else case FFI.representation proxy of
      FFI.Int -> registry
      FFI.Float -> registry
      FFI.Qty _ -> registry
      FFI.List -> registerList proxy registry
      FFI.Tuple2 -> registerTuple2 proxy registry
      FFI.Tuple3 -> registerTuple3 proxy registry
      FFI.Tuple4 -> registerTuple4 proxy registry
      FFI.Tuple5 -> registerTuple5 proxy registry
      FFI.Tuple6 -> registerTuple6 proxy registry
      FFI.Maybe -> registerMaybe proxy registry
      FFI.Result -> registerResult proxy registry
      FFI.Class _ -> registry

registerList :: forall a. FFI a => Proxy (List a) -> TypeRegistry -> TypeRegistry
registerList proxy registry = do
  let name = typeName proxy
  let declaration = structDeclaration name ["c_int64", "c_void_p"]
  TypeRegistry.add name declaration registry

registerTuple2 :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> TypeRegistry -> TypeRegistry
registerTuple2 proxy registry = do
  let name = typeName proxy
  let declaration = structDeclaration name [typeName @a Proxy, typeName @b Proxy]
  registry
    |> registerType @a Proxy
    |> registerType @b Proxy
    |> TypeRegistry.add name declaration

registerTuple3 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Proxy (a, b, c) ->
  TypeRegistry ->
  TypeRegistry
registerTuple3 proxy registry = do
  let name = typeName proxy
  let declaration =
        structDeclaration name $
          [ typeName @a Proxy
          , typeName @b Proxy
          , typeName @c Proxy
          ]
  registry
    |> registerType @a Proxy
    |> registerType @b Proxy
    |> registerType @c Proxy
    |> TypeRegistry.add name declaration

registerTuple4 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Proxy (a, b, c, d) ->
  TypeRegistry ->
  TypeRegistry
registerTuple4 proxy registry = do
  let name = typeName proxy
  let declaration =
        structDeclaration name $
          [ typeName @a Proxy
          , typeName @b Proxy
          , typeName @c Proxy
          , typeName @d Proxy
          ]
  registry
    |> registerType @a Proxy
    |> registerType @b Proxy
    |> registerType @c Proxy
    |> registerType @d Proxy
    |> TypeRegistry.add name declaration

registerTuple5 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Proxy (a, b, c, d, e) ->
  TypeRegistry ->
  TypeRegistry
registerTuple5 proxy registry = do
  let name = typeName proxy
  let declaration =
        structDeclaration name $
          [ typeName @a Proxy
          , typeName @b Proxy
          , typeName @c Proxy
          , typeName @d Proxy
          , typeName @e Proxy
          ]
  registry
    |> registerType @a Proxy
    |> registerType @b Proxy
    |> registerType @c Proxy
    |> registerType @d Proxy
    |> registerType @e Proxy
    |> TypeRegistry.add name declaration

registerTuple6 ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Proxy (a, b, c, d, e, f) ->
  TypeRegistry ->
  TypeRegistry
registerTuple6 proxy registry = do
  let name = typeName proxy
  let declaration =
        structDeclaration name $
          [ typeName @a Proxy
          , typeName @b Proxy
          , typeName @c Proxy
          , typeName @d Proxy
          , typeName @e Proxy
          , typeName @f Proxy
          ]
  registry
    |> registerType @a Proxy
    |> registerType @b Proxy
    |> registerType @c Proxy
    |> registerType @d Proxy
    |> registerType @e Proxy
    |> registerType @f Proxy
    |> TypeRegistry.add name declaration

registerMaybe :: forall a. FFI a => Proxy (Maybe a) -> TypeRegistry -> TypeRegistry
registerMaybe proxy registry = do
  let name = typeName proxy
  let declaration = structDeclaration name ["c_int64", typeName @a Proxy]
  registry
    |> registerType @a Proxy
    |> TypeRegistry.add name declaration

registerResult :: forall a x. FFI a => Proxy (Result x a) -> TypeRegistry -> TypeRegistry
registerResult proxy registry = do
  let name = typeName proxy
  let declaration = structDeclaration name ["c_int64", "_ErrorMessage", typeName @a Proxy]
  registry
    |> registerType @a Proxy
    |> TypeRegistry.add name declaration

invoke :: Int -> Int -> Text -> Text -> Text
invoke classId functionId inputPtr outputPtr =
  Python.call "_lib.opensolid_invoke" [Text.int classId, Text.int functionId, inputPtr, outputPtr]
