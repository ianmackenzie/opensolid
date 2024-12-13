module Python.MemberFunction (definition) where

import API.MemberFunction (MemberFunction (..))
import API.MemberFunction qualified as MemberFunction
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

definition :: FFI.Id value -> (Name, MemberFunction value) -> Text
definition classId (functionName, memberFunction) = do
  let ffiFunctionName = MemberFunction.ffiName classId functionName memberFunction
  let (maybeConstraint, arguments, selfType, returnType) = MemberFunction.signature memberFunction
  let functionArgument (argName, argType) = FFI.snakeCase argName + ": " + Python.Type.qualifiedName argType
  let functionArguments = Text.join "," ("self" : List.map functionArgument arguments)
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments = List.map (Pair.mapFirst FFI.snakeCase) arguments
  let selfArgument = ("self", selfType)
  Python.lines
    [ "def " + FFI.snakeCase functionName + "(" + functionArguments + ") -> " + Python.Type.qualifiedName returnType + ":"
    , Python.indent
        [ Python.docstring (MemberFunction.documentation memberFunction)
        , "inputs = " + Python.FFI.argumentValue (maybeToleranceArgument + normalArguments + [selfArgument])
        , "output = " + Python.FFI.dummyValue returnType
        , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
        , "return " + Python.FFI.outputValue returnType "output"
        ]
    ]
