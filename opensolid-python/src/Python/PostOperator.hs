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

definition :: FFI.Id value -> (BinaryOperator.Id, List (PostOperator value)) -> Text
definition classId (operatorId, operators) = do
  case List.map (overload classId operatorId) operators of
    [(signature, _, body)] -> Python.lines [signature, Python.indent [body]]
    overloads -> do
      let overloadDeclaration (signature, _, _) = Python.Function.overloadDeclaration signature
      let overloadCase (_, matchPattern, body) = Python.Function.overloadCase matchPattern [body]
      Python.lines
        [ Python.lines (List.map overloadDeclaration overloads)
        , "def " + functionName operatorId + "(self, *args, **keywords):"
        , Python.indent
            [ "match (args, keywords):"
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

overload :: FFI.Id value -> BinaryOperator.Id -> PostOperator value -> (Text, Text, Text)
overload classId operatorId memberFunction = do
  let ffiFunctionName = PostOperator.ffiName classId operatorId memberFunction
  let (selfType, rhsType, returnType) = PostOperator.signature memberFunction
  let signature = signatureN operatorId rhsType returnType
  let matchPattern = Python.Function.matchPattern [(PostOperator.rhsName, rhsType)]
  let body = bodyN ffiFunctionName selfType rhsType returnType
  (signature, matchPattern, body)

rhsArgName :: Text
rhsArgName = Name.snakeCase PostOperator.rhsName

signatureN :: BinaryOperator.Id -> FFI.Type -> FFI.Type -> Text
signatureN operatorId rhsType returnType = do
  let rhsTypeName = Python.Type.qualifiedName rhsType
  let returnTypeName = Python.Type.qualifiedName returnType
  let rhsArgument = rhsArgName + ": " + rhsTypeName
  "def " + functionName operatorId + "(self, " + rhsArgument + ") -> " + returnTypeName + ":"

bodyN :: Text -> FFI.Type -> FFI.Type -> FFI.Type -> Text
bodyN ffiFunctionName selfType rhsType returnType =
  Python.lines
    [ "inputs = " + Python.FFI.argumentValue [("self", selfType), (rhsArgName, rhsType)]
    , "output = " + Python.FFI.dummyValue returnType
    , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
    , "return " + Python.FFI.outputValue returnType "output"
    ]