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

definition :: Text -> (Name, List StaticFunction) -> Text
definition functionPrefix (functionName, staticFunctions) = do
  case List.map (overload functionPrefix functionName) staticFunctions of
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

overload :: Text -> Name -> StaticFunction -> (Text, Text, Text)
overload functionPrefix functionName staticFunction = do
  let ffiFunctionName = functionPrefix + StaticFunction.ffiName functionName staticFunction
  let (maybeConstraint, arguments, returnType) = StaticFunction.signature staticFunction
  let signature = signatureN functionName arguments (Python.Type.name returnType)
  let matchPattern = Python.Function.matchPattern arguments
  let body = bodyN ffiFunctionName maybeConstraint arguments returnType
  (signature, matchPattern, body)

signatureN :: Name -> List (Name, FFI.Type) -> Text -> Text
signatureN functionName args returnType = do
  let functionArgument (argName, argType) = Name.snakeCase argName + ": " + Python.Type.name argType
  let functionArguments = Text.join "," (List.map functionArgument args)
  Python.lines
    [ "@staticmethod"
    , "def " + Name.snakeCase functionName + "(" + functionArguments + ") -> " + returnType + ":"
    ]

bodyN :: Text -> Maybe Constraint -> List (Name, FFI.Type) -> FFI.Type -> Text
bodyN ffiFunctionName maybeConstraint arguments returnType = do
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments = List.map (Pair.mapFirst Name.snakeCase) arguments
  Python.lines
    [ "inputs = " + Python.FFI.argumentValue (maybeToleranceArgument + normalArguments)
    , "output = " + Python.FFI.dummyValue returnType
    , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
    , "return " + Python.FFI.outputValue returnType "output"
    ]
