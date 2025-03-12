module Python.PostOperator (definition) where

import API.BinaryOperator qualified as BinaryOperator
import API.PostOperator (PostOperator (..))
import API.PostOperator qualified as PostOperator
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
import Python.Type qualified

rhsArgName :: Text
rhsArgName = FFI.snakeCase PostOperator.rhsName

definition :: FFI.Id value -> (BinaryOperator.Id, List (PostOperator value)) -> Text
definition classId (operatorId, operators) = do
  case List.map (overload classId operatorId) operators of
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
        , "def " <> functionName operatorId <> "(self, " <> rhsArgName <> "):"
        , Python.indent
            [ documentation operatorId
            , "match " <> rhsArgName <> ":"
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
      BinaryOperator.Add -> "Return ``self <> " <> rhsArgName <> "``."
      BinaryOperator.Sub -> "Return ``self - " <> rhsArgName <> "``."
      BinaryOperator.Mul -> "Return ``self * " <> rhsArgName <> "``."
      BinaryOperator.Div -> "Return ``self / " <> rhsArgName <> "``."
      BinaryOperator.FloorDiv -> "Return ``self // " <> rhsArgName <> "``."
      BinaryOperator.Mod -> "Return ``self % " <> rhsArgName <> "``."
      BinaryOperator.Dot -> "Compute the dot product of two vector-like values."
      BinaryOperator.Cross -> "Compute the cross product of two vector-like values."

functionName :: BinaryOperator.Id -> Text
functionName operatorId = case operatorId of
  BinaryOperator.Add -> "__add__"
  BinaryOperator.Sub -> "__sub__"
  BinaryOperator.Mul -> "__mul__"
  BinaryOperator.Div -> "__truediv__"
  BinaryOperator.FloorDiv -> "__floordiv__"
  BinaryOperator.Mod -> "__mod__"
  BinaryOperator.Dot -> "dot"
  BinaryOperator.Cross -> "cross"

overload :: FFI.Id value -> BinaryOperator.Id -> PostOperator value -> (Text, Text, Text)
overload classId operatorId memberFunction = do
  let ffiFunctionName = PostOperator.ffiName classId operatorId memberFunction
  let (selfType, rhsType, returnType) = PostOperator.signature memberFunction
  let signature = overloadSignature operatorId rhsType returnType
  let matchPattern = Python.Function.typePattern rhsType
  let body = overloadBody ffiFunctionName selfType rhsType returnType
  (signature, matchPattern, body)

overloadSignature :: BinaryOperator.Id -> FFI.Type -> FFI.Type -> Text
overloadSignature operatorId rhsType returnType = do
  let rhsTypeName = Python.Type.qualifiedName rhsType
  let returnTypeName = Python.Type.qualifiedName returnType
  let rhsArgument = rhsArgName <> ": " <> rhsTypeName
  "def " <> functionName operatorId <> "(self, " <> rhsArgument <> ") -> " <> returnTypeName <> ":"

overloadBody :: Text -> FFI.Type -> FFI.Type -> FFI.Type -> Text
overloadBody ffiFunctionName selfType rhsType returnType =
  Python.Function.body ffiFunctionName [("self", selfType), (rhsArgName, rhsType)] returnType
