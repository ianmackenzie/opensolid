module Python.StaticFunction (definition) where

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

definition :: FFI.Id a -> (Name, StaticFunction) -> Text
definition classId (functionName, staticFunction) = do
  let ffiFunctionName = StaticFunction.ffiName classId functionName staticFunction
  let (maybeConstraint, arguments, returnType) = StaticFunction.signature staticFunction
  let functionArgument (argName, argType) = FFI.snakeCase argName + ": " + Python.Type.qualifiedName argType
  let functionArguments = Text.join "," (List.map functionArgument arguments)
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments = List.map (Pair.mapFirst FFI.snakeCase) arguments
  Python.lines
    [ "@staticmethod"
    , "def " + FFI.snakeCase functionName + "(" + functionArguments + ") -> " + Python.Type.qualifiedName returnType + ":"
    , Python.indent
        [ Python.docstring (StaticFunction.documentation staticFunction)
        , "inputs = " + Python.FFI.argumentValue (maybeToleranceArgument + normalArguments)
        , "output = " + Python.FFI.dummyValue returnType
        , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
        , "return " + Python.FFI.outputValue returnType "output"
        ]
    ]
