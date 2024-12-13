module Python.StaticFunction (definition) where

import API.Constraint (Constraint)
import API.StaticFunction (StaticFunction (..))
import API.StaticFunction qualified as StaticFunction
import List qualified
import Maybe qualified
import OpenSolid
import OpenSolid.FFI (Name)
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Python qualified
import Python.FFI qualified
import Python.Function qualified
import Python.Type qualified
import Text qualified

argumentCount :: StaticFunction -> Int
argumentCount staticFunction = do
  let (_, arguments, _) = StaticFunction.signature staticFunction
  List.length arguments

definition :: FFI.Id a -> (Name, List StaticFunction) -> Text
definition classId (functionName, staticFunctions) = do
  let sortedFunctions = List.reverse (List.sortBy argumentCount staticFunctions)
  case List.map (overload classId functionName) sortedFunctions of
    [(signature, _, body, documentation)] ->
      Python.lines
        [ signature
        , Python.indent
            [ Python.docstring documentation
            , body
            ]
        ]
    overloads -> do
      let overloadDeclaration (signature, _, _, _) = Python.Function.overloadDeclaration signature
      let overloadCase (_, matchPattern, body, _) = Python.Function.overloadCase matchPattern [body]
      Python.lines
        [ Python.lines (List.map overloadDeclaration overloads)
        , ""
        , "@staticmethod"
        , "def " + FFI.snakeCase functionName + "(*args, **keywords):"
        , Python.indent
            [ "match (args, keywords):"
            , Python.indent
                [ Python.lines (List.map overloadCase overloads)
                , "case _:"
                , Python.indent
                    [ "message = 'Unexpected function arguments'"
                    , "raise TypeError(message)"
                    ]
                ]
            ]
        ]

overload :: FFI.Id a -> Name -> StaticFunction -> (Text, Text, Text, Text)
overload classId functionName staticFunction = do
  let ffiFunctionName = StaticFunction.ffiName classId functionName staticFunction
  let (maybeConstraint, arguments, returnType) = StaticFunction.signature staticFunction
  let signature = overloadSignature functionName arguments (Python.Type.qualifiedName returnType)
  let matchPattern = Python.Function.matchPattern arguments
  let body = overloadBody ffiFunctionName maybeConstraint arguments returnType
  (signature, matchPattern, body, StaticFunction.documentation staticFunction)

overloadSignature :: Name -> List (Name, FFI.Type) -> Text -> Text
overloadSignature functionName args returnType = do
  let functionArgument (argName, argType) = FFI.snakeCase argName + ": " + Python.Type.qualifiedName argType
  let functionArguments = Text.join "," (List.map functionArgument args)
  Python.lines
    [ "@staticmethod"
    , "def " + FFI.snakeCase functionName + "(" + functionArguments + ") -> " + returnType + ":"
    ]

overloadBody :: Text -> Maybe Constraint -> List (Name, FFI.Type) -> FFI.Type -> Text
overloadBody ffiFunctionName maybeConstraint arguments returnType = do
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments = List.map (Pair.mapFirst FFI.snakeCase) arguments
  Python.lines
    [ "inputs = " + Python.FFI.argumentValue (maybeToleranceArgument + normalArguments)
    , "output = " + Python.FFI.dummyValue returnType
    , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
    , "return " + Python.FFI.outputValue returnType "output"
    ]
