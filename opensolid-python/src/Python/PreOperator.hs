module Python.PreOperator (definition) where

import API.BinaryOperator qualified as BinaryOperator
import API.PreOperator (PreOperator (..))
import API.PreOperator qualified as PreOperator
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
import Python.Type qualified

lhsArgName :: Text
lhsArgName = FFI.snakeCase PreOperator.lhsName

definition :: FFI.Class -> (BinaryOperator.Id, List PreOperator) -> Text
definition ffiClass (operatorId, operators) = do
  case List.map (overload ffiClass operatorId) operators of
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

overload :: FFI.Class -> BinaryOperator.Id -> PreOperator -> (Text, Text, Text)
overload ffiClass operatorId memberFunction = do
  let ffiFunctionName = PreOperator.ffiName ffiClass operatorId memberFunction
  let selfType = FFI.Class ffiClass
  let (lhsType, returnType) = PreOperator.signature memberFunction
  let signature = overloadSignature operatorId lhsType returnType
  let matchPattern = Python.Function.typePattern lhsType
  let body = overloadBody ffiFunctionName selfType lhsType returnType
  (signature, matchPattern, body)

overloadSignature :: BinaryOperator.Id -> FFI.Type -> FFI.Type -> Text
overloadSignature operatorId lhsType returnType = do
  let lhsTypeName = Python.Type.qualifiedName lhsType
  let returnTypeName = Python.Type.qualifiedName returnType
  let lhsArgument = lhsArgName <> ": " <> lhsTypeName
  "def " <> functionName operatorId <> "(self, " <> lhsArgument <> ") -> " <> returnTypeName <> ":"

overloadBody :: Text -> FFI.Type -> FFI.Type -> FFI.Type -> Text
overloadBody ffiFunctionName selfType lhsType returnType =
  Python.Function.body ffiFunctionName [(lhsArgName, lhsType), ("self", selfType)] returnType
