module Python.StaticFunction (definition) where

import OpenSolid.API.StaticFunction (StaticFunction (..))
import OpenSolid.API.StaticFunction qualified as StaticFunction
import OpenSolid.FFI (Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
import Python.Type qualified

definition :: FFI.ClassName -> (Name, StaticFunction) -> Text
definition className (functionName, staticFunction) = do
  let ffiFunctionName = StaticFunction.ffiName className functionName staticFunction
  let (maybeImplicitArgument, positionalArguments, namedArguments, returnType) =
        StaticFunction.signature staticFunction
  let maybeImplicitValue = Maybe.map Python.Function.implicitValue maybeImplicitArgument
  let normalArguments =
        List.map (Pair.mapFirst FFI.snakeCase) (positionalArguments <> namedArguments)
  let ffiArguments = List.maybe maybeImplicitValue <> normalArguments
  Python.lines
    [ "@staticmethod"
    , "def "
        <> FFI.snakeCase functionName
        <> "("
        <> Python.Function.arguments (#includeSelf False) positionalArguments namedArguments
        <> ") -> "
        <> Python.Type.qualifiedName returnType
        <> ":"
    , Python.indent
        [ Python.docstring (StaticFunction.documentation staticFunction)
        , Python.Function.body ffiFunctionName ffiArguments returnType
        ]
    ]
