module Python.PostOperator (definition) where

import List qualified
import OpenSolid
import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.Name qualified as Name
import OpenSolid.API.PostOperator (PostOperator (..))
import OpenSolid.API.PostOperator qualified as PostOperator
import OpenSolid.FFI qualified as FFI
import Python qualified
import Python.FFI qualified
import Python.Function qualified
import Python.Type qualified

rhsArgName :: Text
rhsArgName = Name.snakeCase PostOperator.rhsName

definition :: FFI.Id value -> (BinaryOperator.Id, List (PostOperator value)) -> Text
definition classId (operatorId, operators) = do
  case List.map (overload classId operatorId) operators of
    [(signature, _, body)] -> Python.lines [signature, Python.indent [body]]
    overloads -> do
      let overloadDeclaration (signature, _, _) = Python.Function.overloadDeclaration signature
      let overloadCase (_, matchPattern, body) = Python.Function.overloadCase matchPattern [body]
      Python.lines
        [ Python.lines (List.map overloadDeclaration overloads)
        , "def " + functionName operatorId + "(self, " + rhsArgName + "):"
        , Python.indent
            [ "match " + rhsArgName + ":"
            , Python.indent
                [ Python.lines (List.map overloadCase overloads)
                , "case _:"
                , Python.indent ["return NotImplemented"]
                ]
            ]
        ]

functionName :: BinaryOperator.Id -> Text
functionName operatorId = case operatorId of
  BinaryOperator.Add -> "__add__"
  BinaryOperator.Sub -> "__sub__"
  BinaryOperator.Mul -> "__mul__"
  BinaryOperator.Div -> "__truediv__"
  BinaryOperator.FloorDiv -> "__floordiv__"
  BinaryOperator.Mod -> "__mod__"

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
  let rhsArgument = rhsArgName + ": " + rhsTypeName
  "def " + functionName operatorId + "(self, " + rhsArgument + ") -> " + returnTypeName + ":"

overloadBody :: Text -> FFI.Type -> FFI.Type -> FFI.Type -> Text
overloadBody ffiFunctionName selfType rhsType returnType =
  Python.lines
    [ "inputs = " + Python.FFI.argumentValue [("self", selfType), (rhsArgName, rhsType)]
    , "output = " + Python.FFI.dummyValue returnType
    , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
    , "return " + Python.FFI.outputValue returnType "output"
    ]
