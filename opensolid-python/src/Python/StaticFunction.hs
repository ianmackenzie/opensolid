module Python.StaticFunction (definition) where

import API.StaticFunction (StaticFunction (..))
import API.StaticFunction qualified as StaticFunction
import OpenSolid.FFI (Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
import Python.Type qualified

definition :: FFI.Class -> (Name, StaticFunction) -> Text
definition ffiClass (functionName, staticFunction) = do
  let ffiFunctionName = StaticFunction.ffiName ffiClass functionName staticFunction
  let (maybeConstraint, positionalArguments, namedArguments, returnType) =
        StaticFunction.signature staticFunction
  let functionArguments = Python.Function.arguments False positionalArguments namedArguments
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments =
        List.map (Pair.mapFirst FFI.snakeCase) (positionalArguments <> namedArguments)
  let ffiArguments = List.maybe maybeToleranceArgument <> normalArguments
  Python.lines
    [ "@staticmethod"
    , "def " <> FFI.snakeCase functionName <> "(" <> functionArguments <> ") -> " <> Python.Type.qualifiedName returnType <> ":"
    , Python.indent
        [ Python.docstring (StaticFunction.documentation staticFunction)
        , Python.Function.body ffiFunctionName ffiArguments returnType
        ]
    ]
