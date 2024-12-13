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
import Python.Function qualified
import Python.Type qualified
import Text qualified

definition :: FFI.Id a -> (Name, StaticFunction) -> Text
definition classId (functionName, staticFunction) = do
  let ffiFunctionName = StaticFunction.ffiName classId functionName staticFunction
  let (maybeConstraint, arguments, returnType) = StaticFunction.signature staticFunction
  let functionArguments = Text.join "," (List.map Python.Function.argument arguments)
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments = List.map (Pair.mapFirst FFI.snakeCase) arguments
  let allArguments = maybeToleranceArgument + normalArguments
  Python.lines
    [ "@staticmethod"
    , "def " + FFI.snakeCase functionName + "(" + functionArguments + ") -> " + Python.Type.qualifiedName returnType + ":"
    , Python.indent
        [ Python.docstring (StaticFunction.documentation staticFunction)
        , Python.Function.body ffiFunctionName allArguments returnType
        ]
    ]
