module Python.StaticFunction (definition) where

import List qualified
import Maybe qualified
import OpenSolid
import OpenSolid.API.Constraint (Constraint)
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.API.StaticFunction (StaticFunction (..))
import OpenSolid.API.StaticFunction qualified as StaticFunction
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Python qualified
import Python.FFI qualified
import Python.Function qualified
import Python.Type qualified
import Text qualified

definition :: FFI.Id a -> (Name, List StaticFunction) -> Text
definition classId (functionName, staticFunctions) = do
  case List.map (overload classId functionName) staticFunctions of
    [(signature, _, body)] -> Python.lines [signature, Python.indent [body]]
    overloads -> do
      let overloadDeclaration (signature, _, _) = Python.Function.overloadDeclaration signature
      let overloadCase (_, matchPattern, body) = Python.Function.overloadCase matchPattern [body]
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
                , Python.indent
                    [ "message = \"Unexpected function arguments\""
                    , "raise TypeError(message)"
                    ]
                ]
            ]
        ]

overload :: FFI.Id a -> Name -> StaticFunction -> (Text, Text, Text)
overload classId functionName staticFunction = do
  let ffiFunctionName = StaticFunction.ffiName classId functionName staticFunction
  let (maybeConstraint, arguments, returnType) = StaticFunction.signature staticFunction
  let signature = overloadSignature functionName arguments (Python.Type.qualifiedName returnType)
  let matchPattern = Python.Function.matchPattern arguments
  let body = overloadBody ffiFunctionName maybeConstraint arguments returnType
  (signature, matchPattern, body)

overloadSignature :: Name -> List (Name, FFI.Type) -> Text -> Text
overloadSignature functionName args returnType = do
  let functionArgument (argName, argType) = Name.snakeCase argName + ": " + Python.Type.qualifiedName argType
  let functionArguments = Text.join "," (List.map functionArgument args)
  Python.lines
    [ "@staticmethod"
    , "def " + Name.snakeCase functionName + "(" + functionArguments + ") -> " + returnType + ":"
    ]

overloadBody :: Text -> Maybe Constraint -> List (Name, FFI.Type) -> FFI.Type -> Text
overloadBody ffiFunctionName maybeConstraint arguments returnType = do
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments = List.map (Pair.mapFirst Name.snakeCase) arguments
  Python.lines
    [ "inputs = " + Python.FFI.argumentValue (maybeToleranceArgument + normalArguments)
    , "output = " + Python.FFI.dummyValue returnType
    , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
    , "return " + Python.FFI.outputValue returnType "output"
    ]
