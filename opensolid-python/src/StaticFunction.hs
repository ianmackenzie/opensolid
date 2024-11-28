{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module StaticFunction (definition) where

import CTypes qualified
import Data.Proxy (Proxy (Proxy))
import Function qualified
import List qualified
import OpenSolid
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.API.StaticFunction (StaticFunction (..))
import OpenSolid.API.StaticFunction qualified as StaticFunction
import OpenSolid.FFI (FFI)
import Python qualified
import Text qualified
import Type qualified

definition :: Text -> (Name, List StaticFunction) -> Text
definition functionPrefix (functionName, staticFunctions) = do
  case List.map (overload functionPrefix functionName) staticFunctions of
    [(signature, _, body)] -> Python.lines [signature, Python.indent body]
    overloads -> do
      let overloadDeclaration (signature, _, _) = Function.overloadDeclaration signature
      let overloadCase (_, matchPattern, body) = Function.overloadCase matchPattern body
      Python.lines
        [ Python.lines (List.map overloadDeclaration overloads)
        , ""
        , "@staticmethod"
        , "def " + Name.snakeCase functionName + "(*args, **keywords):"
        , Python.indent
            [ "match (args, keywords):"
            , Python.indent
                [ Python.lines (List.map overloadCase overloads)
                , "case _:"
                , "    message = \"Unexpected function arguments\""
                , "    raise TypeError(message)"
                ]
            ]
        ]

overload :: Text -> Name -> StaticFunction -> (Text, Text, List Text)
overload functionPrefix functionName staticFunction = do
  let ffiFunctionName = functionPrefix + StaticFunction.ffiName functionName staticFunction
  case staticFunction of
    StaticFunction0 v ->
      ( signature0 functionName v
      , Function.matchPattern0
      , body0 ffiFunctionName v
      )
    StaticFunction1 argName1 f ->
      ( signature1 functionName argName1 f
      , Function.matchPattern1 argName1 f
      , body1 ffiFunctionName argName1 f
      )
    StaticFunction2 argName1 argName2 f ->
      ( signature2 functionName argName1 argName2 f
      , Function.matchPattern2 argName1 argName2 f
      , body2 ffiFunctionName argName1 argName2 f
      )
    StaticFunction3 argName1 argName2 argName3 f ->
      ( signature3 functionName argName1 argName2 argName3 f
      , Function.matchPattern3 argName1 argName2 argName3 f
      , body3 ffiFunctionName argName1 argName2 argName3 f
      )
    StaticFunction4 argName1 argName2 argName3 argName4 f ->
      ( signature4 functionName argName1 argName2 argName3 argName4 f
      , Function.matchPattern4 argName1 argName2 argName3 argName4 f
      , body4 ffiFunctionName argName1 argName2 argName3 argName4 f
      )
    StaticFunction0U{} -> TODO -- Static functions with non-trivial constraints
    StaticFunction0M{} -> TODO -- Static functions with non-trivial constraints
    StaticFunction1U{} -> TODO -- Static functions with non-trivial constraints
    StaticFunction1M{} -> TODO -- Static functions with non-trivial constraints
    StaticFunction2U{} -> TODO -- Static functions with non-trivial constraints
    StaticFunction2M{} -> TODO -- Static functions with non-trivial constraints
    StaticFunction3U{} -> TODO -- Static functions with non-trivial constraints
    StaticFunction3M{} -> TODO -- Static functions with non-trivial constraints
    StaticFunction4U{} -> TODO -- Static functions with non-trivial constraints
    StaticFunction4M{} -> TODO -- Static functions with non-trivial constraints

signatureN :: Name -> List (Name, Text) -> Text -> Text
signatureN functionName arguments returnType = do
  let functionArgument (argName, argType) = Name.snakeCase argName + ": " + argType
  let functionArguments = Text.join "," (List.map functionArgument arguments)
  Python.lines
    [ "@staticmethod"
    , "def " + Name.snakeCase functionName + "(" + functionArguments + ") -> " + returnType + ":"
    ]

signature0 :: forall a. FFI a => Name -> a -> Text
signature0 functionName _ =
  signatureN functionName [] (Type.name @a Proxy)

signature1 :: forall a b. (FFI a, FFI b) => Name -> Name -> (a -> b) -> Text
signature1 functionName argName1 _ =
  signatureN functionName [(argName1, Type.name @a Proxy)] (Type.name @b Proxy)

signature2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> c) ->
  Text
signature2 functionName argName1 argName2 _ =
  signatureN
    functionName
    [ (argName1, Type.name @a Proxy)
    , (argName2, Type.name @b Proxy)
    ]
    (Type.name @c Proxy)

signature3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d) ->
  Text
signature3 functionName argName1 argName2 argName3 _ =
  signatureN
    functionName
    [ (argName1, Type.name @a Proxy)
    , (argName2, Type.name @b Proxy)
    , (argName3, Type.name @c Proxy)
    ]
    (Type.name @d Proxy)

signature4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> e) ->
  Text
signature4 functionName argName1 argName2 argName3 argName4 _ =
  signatureN
    functionName
    [ (argName1, Type.name @a Proxy)
    , (argName2, Type.name @b Proxy)
    , (argName3, Type.name @c Proxy)
    , (argName4, Type.name @c Proxy)
    ]
    (Type.name @d Proxy)

body0 :: forall a. FFI a => Text -> a -> List Text
body0 ffiFunctionName _ =
  [ "output = " + CTypes.dummyValue @a Proxy
  , CTypes.invoke ffiFunctionName "c_void_p()" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @a Proxy "output"
  ]

body1 :: forall a b. (FFI a, FFI b) => Text -> Name -> (a -> b) -> List Text
body1 ffiFunctionName argName1 _ =
  [ "inputs = " + CTypes.argumentValue1 @a Proxy (Name.snakeCase argName1)
  , "output = " + CTypes.dummyValue @b Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @b Proxy "output"
  ]

body2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Text ->
  Name ->
  Name ->
  (a -> b -> c) ->
  List Text
body2 ffiFunctionName argName1 argName2 _ =
  [ "inputs = "
      + CTypes.argumentValue2 @a @b
        Proxy
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
  , "output = " + CTypes.dummyValue @c Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @c Proxy "output"
  ]

body3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Text ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d) ->
  List Text
body3 ffiFunctionName argName1 argName2 argName3 _ =
  [ "inputs = "
      + CTypes.argumentValue3 @a @b @c
        Proxy
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        (Name.snakeCase argName3)
  , "output = " + CTypes.dummyValue @d Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @d Proxy "output"
  ]

body4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Text ->
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> e) ->
  List Text
body4 ffiFunctionName argName1 argName2 argName3 argName4 _ =
  [ "inputs = "
      + CTypes.argumentValue4 @a @b @c @d
        Proxy
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        (Name.snakeCase argName3)
        (Name.snakeCase argName4)
  , "output = " + CTypes.dummyValue @e Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @e Proxy "output"
  ]
