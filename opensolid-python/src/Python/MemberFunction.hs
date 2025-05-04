module Python.MemberFunction (definition) where

import API.MemberFunction (MemberFunction (..))
import API.MemberFunction qualified as MemberFunction
import OpenSolid.FFI (Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
import Python.Type qualified

definition :: FFI.Class -> (Name, MemberFunction value) -> Text
definition ffiClass (functionName, memberFunction) = do
  let ffiFunctionName = MemberFunction.ffiName ffiClass functionName memberFunction
  let selfType = FFI.Class ffiClass
  let (maybeConstraint, positionalArguments, namedArguments, returnType) =
        MemberFunction.signature memberFunction
  let functionArguments = Python.Function.arguments True positionalArguments namedArguments
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments =
        List.map (Pair.mapFirst FFI.snakeCase) (positionalArguments <> namedArguments)
  let selfArgument = ("self", selfType)
  let ffiArguments = List.maybe maybeToleranceArgument <> normalArguments <> [selfArgument]
  Python.lines
    [ "def " <> FFI.snakeCase functionName <> "(" <> functionArguments <> ") -> " <> Python.Type.qualifiedName returnType <> ":"
    , Python.indent
        [ Python.docstring (MemberFunction.documentation memberFunction)
        , Python.Function.body ffiFunctionName ffiArguments returnType
        ]
    ]
