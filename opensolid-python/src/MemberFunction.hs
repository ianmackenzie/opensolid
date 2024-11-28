module MemberFunction (definition) where

import CTypes qualified
import Data.Proxy (Proxy (Proxy))
import Function qualified
import Length (Length)
import List qualified
import OpenSolid
import OpenSolid.API.MemberFunction (MemberFunction (..))
import OpenSolid.API.MemberFunction qualified as MemberFunction
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI (FFI)
import Python qualified
import Text qualified
import Tolerance qualified
import Type qualified
import Units (Meters)

definition :: forall value. FFI value => Text -> (Name, List (MemberFunction value)) -> Text
definition functionPrefix (functionName, memberFunctions) = do
  case List.map (overload functionPrefix functionName) memberFunctions of
    [(signature, _, body)] -> Python.lines [signature, Python.indent body]
    overloads -> do
      let overloadDeclaration (signature, _, _) = Function.overloadDeclaration signature
      let overloadCase (_, matchPattern, body) = Function.overloadCase matchPattern body
      Python.lines
        [ Python.lines (List.map overloadDeclaration overloads)
        , "def " + Name.snakeCase functionName + "(self, *args, **keywords):"
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

overload :: forall value. FFI value => Text -> Name -> MemberFunction value -> (Text, Text, List Text)
overload functionPrefix functionName memberFunction = do
  let ffiFunctionName = functionPrefix + MemberFunction.ffiName functionName memberFunction
  case memberFunction of
    MemberFunction0 v ->
      ( signature0 functionName v
      , Function.matchPattern0
      , body0N ffiFunctionName v
      )
    MemberFunction1 argName1 f ->
      ( signature1 functionName argName1 f
      , Function.matchPattern1 argName1 f
      , body1N ffiFunctionName argName1 f
      )
    MemberFunction2 argName1 argName2 f ->
      ( signature2 functionName argName1 argName2 f
      , Function.matchPattern2 argName1 argName2 f
      , body2N ffiFunctionName argName1 argName2 f
      )
    MemberFunction3 argName1 argName2 argName3 f ->
      ( signature3 functionName argName1 argName2 argName3 f
      , Function.matchPattern3 argName1 argName2 argName3 f
      , body3N ffiFunctionName argName1 argName2 argName3 f
      )
    MemberFunction4 argName1 argName2 argName3 argName4 f ->
      ( signature4 functionName argName1 argName2 argName3 argName4 f
      , Function.matchPattern4 argName1 argName2 argName3 argName4 f
      , body4N ffiFunctionName argName1 argName2 argName3 argName4 f
      )
    MemberFunction0U v ->
      ( signature0 functionName (Tolerance.exactly v)
      , Function.matchPattern0
      , body0F ffiFunctionName v
      )
    MemberFunction1U argName1 f ->
      ( signature1 functionName argName1 (Tolerance.exactly f)
      , Function.matchPattern1 argName1 (Tolerance.exactly f)
      , body1F ffiFunctionName argName1 f
      )
    MemberFunction2U argName1 argName2 f ->
      ( signature2 functionName argName1 argName2 (Tolerance.exactly f)
      , Function.matchPattern2 argName1 argName2 (Tolerance.exactly f)
      , body2F ffiFunctionName argName1 argName2 f
      )
    MemberFunction3U argName1 argName2 argName3 f ->
      ( signature3 functionName argName1 argName2 argName3 (Tolerance.exactly f)
      , Function.matchPattern3 argName1 argName2 argName3 (Tolerance.exactly f)
      , body3F ffiFunctionName argName1 argName2 argName3 f
      )
    MemberFunction4U argName1 argName2 argName3 argName4 f ->
      ( signature4 functionName argName1 argName2 argName3 argName4 (Tolerance.exactly f)
      , Function.matchPattern4 argName1 argName2 argName3 argName4 (Tolerance.exactly f)
      , body4F ffiFunctionName argName1 argName2 argName3 argName4 f
      )
    MemberFunction0M v ->
      ( signature0 functionName (Tolerance.exactly v)
      , Function.matchPattern0
      , body0L ffiFunctionName v
      )
    MemberFunction1M argName1 f ->
      ( signature1 functionName argName1 (Tolerance.exactly f)
      , Function.matchPattern1 argName1 (Tolerance.exactly f)
      , body1L ffiFunctionName argName1 f
      )
    MemberFunction2M argName1 argName2 f ->
      ( signature2 functionName argName1 argName2 (Tolerance.exactly f)
      , Function.matchPattern2 argName1 argName2 (Tolerance.exactly f)
      , body2L ffiFunctionName argName1 argName2 f
      )
    MemberFunction3M argName1 argName2 argName3 f ->
      ( signature3 functionName argName1 argName2 argName3 (Tolerance.exactly f)
      , Function.matchPattern3 argName1 argName2 argName3 (Tolerance.exactly f)
      , body3L ffiFunctionName argName1 argName2 argName3 f
      )
    MemberFunction4M argName1 argName2 argName3 argName4 f ->
      ( signature4 functionName argName1 argName2 argName3 argName4 (Tolerance.exactly f)
      , Function.matchPattern4 argName1 argName2 argName3 argName4 (Tolerance.exactly f)
      , body4L ffiFunctionName argName1 argName2 argName3 argName4 f
      )

signatureN :: Name -> List (Name, Text) -> Text -> Text
signatureN functionName arguments resultType = do
  let functionArgument (argName, argType) = Name.snakeCase argName + ": " + argType
  let functionArguments = Text.join "," ("self" : List.map functionArgument arguments)
  "def " + Name.snakeCase functionName + "(" + functionArguments + ") -> " + resultType + ":"

signature0 ::
  forall value result.
  (FFI value, FFI result) =>
  Name ->
  (value -> result) ->
  Text
signature0 functionName _ =
  signatureN functionName [] (Type.name @result Proxy)

signature1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  Name ->
  (a -> value -> result) ->
  Text
signature1 functionName argName1 _ =
  signatureN functionName [(argName1, Type.name @a Proxy)] (Type.name @result Proxy)

signature2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> value -> result) ->
  Text
signature2 functionName argName1 argName2 _ =
  signatureN
    functionName
    [ (argName1, Type.name @a Proxy)
    , (argName2, Type.name @b Proxy)
    ]
    (Type.name @result Proxy)

signature3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> value -> result) ->
  Text
signature3 functionName argName1 argName2 argName3 _ =
  signatureN
    functionName
    [ (argName1, Type.name @a Proxy)
    , (argName2, Type.name @b Proxy)
    , (argName3, Type.name @c Proxy)
    ]
    (Type.name @result Proxy)

signature4 ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> value -> result) ->
  Text
signature4 functionName argName1 argName2 argName3 argName4 _ =
  signatureN
    functionName
    [ (argName1, Type.name @a Proxy)
    , (argName2, Type.name @b Proxy)
    , (argName3, Type.name @c Proxy)
    , (argName4, Type.name @d Proxy)
    ]
    (Type.name @result Proxy)

body0N ::
  forall value result.
  (FFI value, FFI result) =>
  Text ->
  (value -> result) ->
  List Text
body0N ffiFunctionName _ =
  [ "inputs = " + CTypes.argumentValue1 @value Proxy "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body1N ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Text ->
  Name ->
  (a -> value -> result) ->
  List Text
body1N ffiFunctionName argName1 _ =
  [ "inputs = " + CTypes.argumentValue2 @a @value Proxy (Name.snakeCase argName1) "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body2N ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Name ->
  Name ->
  (a -> b -> value -> result) ->
  List Text
body2N ffiFunctionName argName1 argName2 _ =
  [ "inputs = "
      + CTypes.argumentValue3 @a @b @value
        Proxy
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body3N ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Text ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> value -> result) ->
  List Text
body3N ffiFunctionName argName1 argName2 argName3 _ =
  [ "inputs = "
      + CTypes.argumentValue4 @a @b @c @value
        Proxy
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        (Name.snakeCase argName3)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body4N ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Text ->
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> value -> result) ->
  List Text
body4N ffiFunctionName argName1 argName2 argName3 argName4 _ =
  [ "inputs = "
      + CTypes.argumentValue5 @a @b @c @d @value
        Proxy
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        (Name.snakeCase argName3)
        (Name.snakeCase argName4)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body0F ::
  forall value result.
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Unitless => value -> result) ->
  List Text
body0F ffiFunctionName _ =
  [ "tolerance = _float_tolerance()"
  , "inputs = " + CTypes.argumentValue2 @Float @value Proxy "tolerance" "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body1F ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Text ->
  Name ->
  (Tolerance Unitless => a -> value -> result) ->
  List Text
body1F ffiFunctionName argName1 _ =
  [ "tolerance = _float_tolerance()"
  , "inputs = "
      + CTypes.argumentValue3 @Float @a @value
        Proxy
        "tolerance"
        (Name.snakeCase argName1)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body2F ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> value -> result) ->
  List Text
body2F ffiFunctionName argName1 argName2 _ =
  [ "tolerance = _float_tolerance()"
  , "inputs = "
      + CTypes.argumentValue4 @Float @a @b @value
        Proxy
        "tolerance"
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body3F ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Text ->
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> value -> result) ->
  List Text
body3F ffiFunctionName argName1 argName2 argName3 _ =
  [ "tolerance = _float_tolerance()"
  , "inputs = "
      + CTypes.argumentValue5 @Float @a @b @c @value
        Proxy
        "tolerance"
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        (Name.snakeCase argName3)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body4F ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Text ->
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> d -> value -> result) ->
  List Text
body4F ffiFunctionName argName1 argName2 argName3 argName4 _ =
  [ "tolerance = _float_tolerance()"
  , "inputs = "
      + CTypes.argumentValue6 @Float @a @b @c @d @value
        Proxy
        "tolerance"
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        (Name.snakeCase argName3)
        (Name.snakeCase argName4)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body0L ::
  forall value result.
  (FFI value, FFI result) =>
  Text ->
  (Tolerance Meters => value -> result) ->
  List Text
body0L ffiFunctionName _ =
  [ "tolerance = _length_tolerance()"
  , "inputs = " + CTypes.argumentValue2 @Length @value Proxy "tolerance" "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body1L ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Text ->
  Name ->
  (Tolerance Meters => a -> value -> result) ->
  List Text
body1L ffiFunctionName argName1 _ =
  [ "tolerance = _length_tolerance()"
  , "inputs = "
      + CTypes.argumentValue3 @Length @a @value
        Proxy
        "tolerance"
        (Name.snakeCase argName1)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body2L ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> value -> result) ->
  List Text
body2L ffiFunctionName argName1 argName2 _ =
  [ "tolerance = _length_tolerance()"
  , "inputs = "
      + CTypes.argumentValue4 @Length @a @b @value
        Proxy
        "tolerance"
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body3L ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Text ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> value -> result) ->
  List Text
body3L ffiFunctionName argName1 argName2 argName3 _ =
  [ "tolerance = _length_tolerance()"
  , "inputs = "
      + CTypes.argumentValue5 @Length @a @b @c @value
        Proxy
        "tolerance"
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        (Name.snakeCase argName3)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]

body4L ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Text ->
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d -> value -> result) ->
  List Text
body4L ffiFunctionName argName1 argName2 argName3 argName4 _ =
  [ "tolerance = _length_tolerance()"
  , "inputs = "
      + CTypes.argumentValue6 @Length @a @b @c @d @value
        Proxy
        "tolerance"
        (Name.snakeCase argName1)
        (Name.snakeCase argName2)
        (Name.snakeCase argName3)
        (Name.snakeCase argName4)
        "self"
  , "output = " + CTypes.dummyValue @result Proxy
  , CTypes.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
  , "return " + CTypes.outputValue @result Proxy "output"
  ]
