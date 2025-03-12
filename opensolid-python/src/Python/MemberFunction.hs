module Python.MemberFunction (definition) where

import API.MemberFunction (MemberFunction (..))
import API.MemberFunction qualified as MemberFunction
import OpenSolid.FFI (Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Python qualified
import Python.Function qualified
import Python.Type qualified

definition :: FFI.Id value -> (Name, MemberFunction value) -> Text
definition classId (functionName, memberFunction) = do
  let ffiFunctionName = MemberFunction.ffiName classId functionName memberFunction
  let (maybeConstraint, arguments, selfType, returnType) = MemberFunction.signature memberFunction
  let functionArguments = Text.join "," ("self" : List.map Python.Function.argument arguments)
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments = List.map (Pair.mapFirst FFI.snakeCase) arguments
  let selfArgument = ("self", selfType)
  let allArguments = List.maybe maybeToleranceArgument <> normalArguments <> [selfArgument]
  Python.lines
    [ "def " <> FFI.snakeCase functionName <> "(" <> functionArguments <> ") -> " <> Python.Type.qualifiedName returnType <> ":"
    , Python.indent
        [ Python.docstring (MemberFunction.documentation memberFunction)
        , Python.Function.body ffiFunctionName allArguments returnType
        ]
    ]
