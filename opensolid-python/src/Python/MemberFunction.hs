module Python.MemberFunction (definition) where

import OpenSolid.API.MemberFunction (MemberFunction (..))
import OpenSolid.API.MemberFunction qualified as MemberFunction
import OpenSolid.FFI (Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
import Python.Type qualified

definition :: FFI.ClassName -> (Name, MemberFunction) -> Text
definition className (functionName, memberFunction) = do
  let ffiFunctionName = MemberFunction.ffiName className functionName memberFunction
  let selfType = FFI.Class className
  let (maybeImplicitArgument, positionalArguments, namedArguments, returnType) =
        MemberFunction.signature memberFunction
  let maybeImplicitValue = Maybe.map Python.Function.implicitValue maybeImplicitArgument
  let normalArguments =
        List.map (Pair.mapFirst FFI.snakeCase) (positionalArguments <> namedArguments)
  let selfArgument = ("self", selfType)
  let ffiArguments = List.maybe maybeImplicitValue <> normalArguments <> [selfArgument]
  Python.lines
    [ "def "
        <> FFI.snakeCase functionName
        <> "("
        <> Python.Function.arguments (#includeSelf True) positionalArguments namedArguments
        <> ") -> "
        <> Python.Type.qualifiedName returnType
        <> ":"
    , Python.indent
        [ Python.docstring (MemberFunction.documentation memberFunction)
        , Python.Function.body ffiFunctionName ffiArguments returnType
        ]
    ]
