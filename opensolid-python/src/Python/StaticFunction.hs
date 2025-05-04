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
import Python.Class qualified
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
  let pyrightHack =
        -- Pyright doesn't seem to realize that it's OK for a static method in a derived class
        -- to have a different signature from a static method with the same name in the base class,
        -- since the function lookup in that case is static, not dynamic.
        -- This causes a problem since 'Direction2d.polar' takes one argument (an angle),
        -- while 'Vector2d.polar' takes two (a magnitude and an angle).
        if Python.Class.qualifiedName ffiClass == "Direction2d"
          && FFI.snakeCase functionName == "polar"
          then " # type: ignore[override]"
          else ""
  Python.lines
    [ "@staticmethod"
    , "def "
        <> FFI.snakeCase functionName
        <> "("
        <> functionArguments
        <> ") -> "
        <> Python.Type.qualifiedName returnType
        <> ":"
        <> pyrightHack
    , Python.indent
        [ Python.docstring (StaticFunction.documentation staticFunction)
        , Python.Function.body ffiFunctionName ffiArguments returnType
        ]
    ]
