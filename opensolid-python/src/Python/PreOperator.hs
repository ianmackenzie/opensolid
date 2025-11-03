module Python.PreOperator (definition) where

import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.ImplicitArgument (ImplicitArgument)
import OpenSolid.API.PreOperatorOverload (PreOperatorOverload (..))
import OpenSolid.API.PreOperatorOverload qualified as PreOperatorOverload
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
import Python.Type qualified

lhsArgName :: Text
lhsArgName = FFI.snakeCase PreOperatorOverload.lhsName

definition :: FFI.ClassName -> (BinaryOperator.Id, List PreOperatorOverload) -> Text
definition className (operatorId, operators) = do
  case List.map (overloadComponents className operatorId) operators of
    [(signature, _, body)] ->
      Python.lines
        [ signature
        , Python.indent
            [ documentation operatorId
            , body
            ]
        ]
    overloads -> do
      let overloadDeclaration (signature, _, _) = Python.Function.overloadDeclaration signature
      let overloadCase (_, matchPattern, body) = Python.Function.overloadCase matchPattern [body]
      Python.lines
        [ Python.lines (List.map overloadDeclaration overloads)
        , "def " <> functionName operatorId <> "(self, " <> lhsArgName <> "):"
        , Python.indent
            [ documentation operatorId
            , "match " <> lhsArgName <> ":"
            , Python.indent
                [ Python.lines (List.map overloadCase overloads)
                , "case _:"
                , Python.indent ["return NotImplemented"]
                ]
            ]
        ]

documentation :: BinaryOperator.Id -> Text
documentation operatorId =
  Python.docstring $
    case operatorId of
      BinaryOperator.Add -> "Return ``" <> lhsArgName <> " <> self``."
      BinaryOperator.Sub -> "Return ``" <> lhsArgName <> " - self``."
      BinaryOperator.Mul -> "Return ``" <> lhsArgName <> " * self``."
      BinaryOperator.Div -> "Return ``" <> lhsArgName <> " / self``."
      BinaryOperator.FloorDiv -> "Return ``" <> lhsArgName <> " // self``."
      BinaryOperator.Mod -> "Return ``" <> lhsArgName <> " % self``."
      BinaryOperator.Dot -> internalError "Dot product should never be a pre-operator"
      BinaryOperator.Cross -> internalError "Cross product should never be a pre-operator"

functionName :: BinaryOperator.Id -> Text
functionName operatorId = case operatorId of
  BinaryOperator.Add -> "__radd__"
  BinaryOperator.Sub -> "__rsub__"
  BinaryOperator.Mul -> "__rmul__"
  BinaryOperator.Div -> "__rtruediv__"
  BinaryOperator.FloorDiv -> "__rfloordiv__"
  BinaryOperator.Mod -> "__rmod__"
  BinaryOperator.Dot -> internalError "Dot product should never be a pre-operator"
  BinaryOperator.Cross -> internalError "Cross product should never be a pre-operator"

overloadComponents ::
  FFI.ClassName ->
  BinaryOperator.Id ->
  PreOperatorOverload ->
  (Text, Text, Text)
overloadComponents className operatorId overload = do
  let ffiFunctionName = PreOperatorOverload.ffiName className operatorId overload
  let selfType = FFI.Class className
  let (maybeImplicitArgument, lhsType, returnType) = PreOperatorOverload.signature overload
  let signature = overloadSignature operatorId lhsType returnType
  let matchPattern = Python.Function.typePattern lhsType
  let body = overloadBody ffiFunctionName maybeImplicitArgument selfType lhsType returnType
  (signature, matchPattern, body)

overloadSignature :: BinaryOperator.Id -> FFI.Type -> FFI.Type -> Text
overloadSignature operatorId lhsType returnType = do
  let lhsTypeName = Python.Type.qualifiedName lhsType
  let returnTypeName = Python.Type.qualifiedName returnType
  let lhsArgument = lhsArgName <> ": " <> lhsTypeName
  "def " <> functionName operatorId <> "(self, " <> lhsArgument <> ") -> " <> returnTypeName <> ":"

overloadBody :: Text -> Maybe ImplicitArgument -> FFI.Type -> FFI.Type -> FFI.Type -> Text
overloadBody ffiFunctionName maybeImplicitArgument selfType lhsType returnType = do
  let maybeImplicitValue = Maybe.map Python.Function.implicitValue maybeImplicitArgument
  let ffiArguments = List.maybe maybeImplicitValue <> [(lhsArgName, lhsType), ("self", selfType)]
  Python.Function.body ffiFunctionName ffiArguments returnType
