{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module StaticFunction (definition) where

import CTypes qualified
import Data.Proxy (Proxy (Proxy))
import Function qualified
import List qualified
import OpenSolid
import OpenSolid.API.Class (Constraint (..), StaticFunction (..))
import OpenSolid.FFI (FFI)
import Python qualified
import Text qualified
import Type qualified

definition :: Int -> Text -> List (Int, StaticFunction) -> Text
definition classId functionName indexedFunctions = do
  case List.map (overload classId functionName) indexedFunctions of
    [(signature, _, body)] -> Python.lines [signature, Python.indent body]
    overloads -> do
      let overloadDeclaration (signature, _, _) = Function.overloadDeclaration signature
      let overloadCase (_, pattern, body) = Function.overloadCase pattern body
      Python.lines
        [ Python.lines (List.map overloadDeclaration overloads)
        , ""
        , "@staticmethod"
        , "def " + Function.name functionName + "(*args, **keywords):"
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

overload :: Int -> Text -> (Int, StaticFunction) -> (Text, Text, List Text)
overload classId functionName (functionId, function) = case function of
  S0 N v ->
    ( signature0 functionName v
    , Function.matchPattern0
    , body0 classId functionId v
    )
  S1 N argName1 f ->
    ( signature1 functionName argName1 f
    , Function.matchPattern1 argName1 f
    , body1 classId functionId argName1 f
    )
  S2 N argName1 argName2 f ->
    ( signature2 functionName argName1 argName2 f
    , Function.matchPattern2 argName1 argName2 f
    , body2 classId functionId argName1 argName2 f
    )
  S3 N argName1 argName2 argName3 f ->
    ( signature3 functionName argName1 argName2 argName3 f
    , Function.matchPattern3 argName1 argName2 argName3 f
    , body3 classId functionId argName1 argName2 argName3 f
    )
  S4 N argName1 argName2 argName3 argName4 f ->
    ( signature4 functionName argName1 argName2 argName3 argName4 f
    , Function.matchPattern4 argName1 argName2 argName3 argName4 f
    , body4 classId functionId argName1 argName2 argName3 argName4 f
    )
  S0{} -> TODO -- Static functions with non-trivial constraints
  S1{} -> TODO -- Static functions with non-trivial constraints
  S2{} -> TODO -- Static functions with non-trivial constraints
  S3{} -> TODO -- Static functions with non-trivial constraints
  S4{} -> TODO -- Static functions with non-trivial constraints

signatureN :: Text -> List (Text, Text) -> Text -> Text
signatureN functionName arguments returnType = do
  let functionArgument (argName, argType) = argName + ": " + argType
  let functionArguments = Text.join "," (List.map functionArgument arguments)
  Python.lines
    [ "@staticmethod"
    , "def " + Function.name functionName + "(" + functionArguments + ") -> " + returnType + ":"
    ]

signature0 :: forall a. FFI a => Text -> a -> Text
signature0 functionName _ =
  signatureN functionName [] (Type.name @a Proxy)

signature1 :: forall a b. (FFI a, FFI b) => Text -> Text -> (a -> b) -> Text
signature1 functionName argName1 _ =
  signatureN functionName [(argName1, Type.name @a Proxy)] (Type.name @b Proxy)

signature2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Text ->
  Text ->
  Text ->
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
  Text ->
  Text ->
  Text ->
  Text ->
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
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
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

body0 :: forall a. FFI a => Int -> Int -> a -> List Text
body0 classId functionId _ =
  [ "output = " + CTypes.dummyValue @a Proxy
  , CTypes.invoke classId functionId "c_void_p()" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @a Proxy "output"
  ]

body1 :: forall a b. (FFI a, FFI b) => Int -> Int -> Text -> (a -> b) -> List Text
body1 classId functionId argName1 _ =
  [ "inputs = " + CTypes.argumentValue1 @a Proxy argName1
  , "output = " + CTypes.dummyValue @b Proxy
  , CTypes.invoke classId functionId "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @b Proxy "output"
  ]

body2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Int ->
  Int ->
  Text ->
  Text ->
  (a -> b -> c) ->
  List Text
body2 classId functionId argName1 argName2 _ =
  [ "inputs = " + CTypes.argumentValue2 @a @b Proxy argName1 argName2
  , "output = " + CTypes.dummyValue @c Proxy
  , CTypes.invoke classId functionId "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @c Proxy "output"
  ]

body3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Int ->
  Int ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d) ->
  List Text
body3 classId functionId argName1 argName2 argName3 _ =
  [ "inputs = " + CTypes.argumentValue3 @a @b @c Proxy argName1 argName2 argName3
  , "output = " + CTypes.dummyValue @d Proxy
  , CTypes.invoke classId functionId "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @d Proxy "output"
  ]

body4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Int ->
  Int ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> e) ->
  List Text
body4 classId functionId argName1 argName2 argName3 argName4 _ =
  [ "inputs = " + CTypes.argumentValue4 @a @b @c @d Proxy argName1 argName2 argName3 argName4
  , "output = " + CTypes.dummyValue @e Proxy
  , CTypes.invoke classId functionId "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @e Proxy "output"
  ]
