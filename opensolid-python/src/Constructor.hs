module Constructor (definition) where

import CTypes qualified
import Data.Proxy (Proxy (Proxy))
import Function qualified
import List qualified
import OpenSolid
import OpenSolid.API.Class.Constructor (Constructor (..))
import OpenSolid.API.Class.Constructor qualified as Constructor
import OpenSolid.API.Constraint (Constraint (..))
import OpenSolid.FFI (FFI)
import Python qualified
import Text qualified
import Type qualified

definition :: Text -> List (Constructor value) -> Text
definition functionPrefix constructors = do
  let ptrOverload =
        ( "def __init__(self, *, __ptr__ : c_void_p) -> None:"
        , "([],{\"__ptr__\": c_void_p() as __ptr__, **rest}) if not rest"
        , ["self.__ptr__ = __ptr__"]
        )
  case ptrOverload : List.map (overload functionPrefix) constructors of
    [(signature, _, body)] -> Python.lines [signature, Python.indent body]
    overloads -> do
      let overloadDeclaration (signature, _, _) = Function.overloadDeclaration signature
      let overloadCase (_, pattern, body) = Function.overloadCase pattern body
      Python.lines
        [ Python.lines (List.map overloadDeclaration overloads)
        , "def __init__(self, *args, **keywords):"
        , Python.indent
            [ "match (args, keywords):"
            , Python.indent
                [ Python.lines (List.map overloadCase overloads)
                , "case _:"
                , "    message = \"Unexpected constructor arguments\""
                , "    raise TypeError(message)"
                ]
            ]
        ]

overload :: Text -> Constructor value -> (Text, Text, List Text)
overload functionPrefix constructor = do
  let ffiFunctionName = functionPrefix + Constructor.ffiName constructor
  case constructor of
    C0 N _ ->
      ( signature0
      , Function.matchPattern0
      , body0 ffiFunctionName
      )
    C1 N argName1 f ->
      ( signature1 argName1 f
      , Function.matchPattern1 argName1 f
      , body1 ffiFunctionName argName1 f
      )
    C2 N argName1 argName2 f ->
      ( signature2 argName1 argName2 f
      , Function.matchPattern2 argName1 argName2 f
      , body2 ffiFunctionName argName1 argName2 f
      )
    C3 N argName1 argName2 argName3 f ->
      ( signature3 argName1 argName2 argName3 f
      , Function.matchPattern3 argName1 argName2 argName3 f
      , body3 ffiFunctionName argName1 argName2 argName3 f
      )
    C4 N argName1 argName2 argName3 argName4 f ->
      ( signature4 argName1 argName2 argName3 argName4 f
      , Function.matchPattern4 argName1 argName2 argName3 argName4 f
      , body4 ffiFunctionName argName1 argName2 argName3 argName4 f
      )
    C0{} -> TODO -- Constructors with non-trivial constraints
    C1{} -> TODO -- Constructors with non-trivial constraints
    C2{} -> TODO -- Constructors with non-trivial constraints
    C3{} -> TODO -- Constructors with non-trivial constraints
    C4{} -> TODO -- Constructors with non-trivial constraints

signatureN :: List (Text, Text) -> Text
signatureN arguments = do
  let typedArgument (argName, argType) = argName + ": " + argType
  "def __init__(self, " + Text.join "," (List.map typedArgument arguments) + ") -> None:"

signature0 :: Text
signature0 = signatureN []

signature1 :: forall a b. (FFI a, FFI b) => Text -> (a -> b) -> Text
signature1 argName1 _ =
  signatureN [(argName1, Type.name @a Proxy)]

signature2 :: forall a b c. (FFI a, FFI b, FFI c) => Text -> Text -> (a -> b -> c) -> Text
signature2 argName1 argName2 _ =
  signatureN [(argName1, Type.name @a Proxy), (argName2, Type.name @b Proxy)]

signature3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d) ->
  Text
signature3 argName1 argName2 argName3 _ =
  signatureN
    [ (argName1, Type.name @a Proxy)
    , (argName2, Type.name @b Proxy)
    , (argName3, Type.name @c Proxy)
    ]

signature4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> e) ->
  Text
signature4 argName1 argName2 argName3 argName4 _ =
  signatureN
    [ (argName1, Type.name @a Proxy)
    , (argName2, Type.name @b Proxy)
    , (argName3, Type.name @c Proxy)
    , (argName4, Type.name @c Proxy)
    ]

body0 :: Text -> List Text
body0 ffiFunctionName =
  [ "self.__ptr__ = c_void_p()"
  , CTypes.invoke ffiFunctionName "c_void_p()" "ctypes.byref(self.__ptr__)"
  ]

body1 :: forall a b. FFI a => Text -> Text -> (a -> b) -> List Text
body1 ffiFunctionName argName1 _ =
  [ "self.__ptr__ = c_void_p()"
  , "inputs = " + CTypes.argumentValue1 @a Proxy argName1
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(self.__ptr__)"
  ]

body2 ::
  forall a b c.
  (FFI a, FFI b) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> c) ->
  List Text
body2 ffiFunctionName argName1 argName2 _ =
  [ "self.__ptr__ = c_void_p()"
  , "inputs = " + CTypes.argumentValue2 @a @b Proxy argName1 argName2
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(self.__ptr__)"
  ]

body3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d) ->
  List Text
body3 ffiFunctionName argName1 argName2 argName3 _ =
  [ "self.__ptr__ = c_void_p()"
  , "inputs = " + CTypes.argumentValue3 @a @b @c Proxy argName1 argName2 argName3
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(self.__ptr__)"
  ]

body4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> e) ->
  List Text
body4 ffiFunctionName argName1 argName2 argName3 argName4 _ =
  [ "self.__ptr__ = c_void_p()"
  , "inputs = " + CTypes.argumentValue4 @a @b @c @d Proxy argName1 argName2 argName3 argName4
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(self.__ptr__)"
  ]
