module Python.PreOperator (definition) where

import List qualified
import OpenSolid
import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.Name qualified as Name
import OpenSolid.API.PreOperator (PreOperator (..))
import OpenSolid.API.PreOperator qualified as PreOperator
import OpenSolid.FFI qualified as FFI
import Python qualified
import Python.FFI qualified
import Python.Function qualified
import Python.Type qualified

definition :: FFI.Id value -> (BinaryOperator.Id, List (PreOperator value)) -> Text
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
  BinaryOperator.Add -> "__radd__"
  BinaryOperator.Sub -> "__rsub__"
  BinaryOperator.Mul -> "__rmul__"
  BinaryOperator.Div -> "__rtruediv__"

overload :: FFI.Id value -> BinaryOperator.Id -> PreOperator value -> (Text, Text, Text)
overload classId operatorId memberFunction = do
  let ffiFunctionName = PreOperator.ffiName classId operatorId memberFunction
  let (lhsType, selfType, returnType) = PreOperator.signature memberFunction
  let signature = signatureN operatorId lhsType returnType
  let matchPattern = Python.Function.matchPattern [(PreOperator.lhsName, lhsType)]
  let body = bodyN ffiFunctionName selfType lhsType returnType
  (signature, matchPattern, body)

lhsArgName :: Text
lhsArgName = Name.snakeCase PreOperator.lhsName

signatureN :: BinaryOperator.Id -> FFI.Type -> FFI.Type -> Text
signatureN operatorId lhsType returnType = do
  let lhsTypeName = Python.Type.qualifiedName lhsType
  let returnTypeName = Python.Type.qualifiedName returnType
  let lhsArgument = lhsArgName + ": " + lhsTypeName
  "def " + functionName operatorId + "(self, " + lhsArgument + ") -> " + returnTypeName + ":"

bodyN :: Text -> FFI.Type -> FFI.Type -> FFI.Type -> Text
bodyN ffiFunctionName selfType lhsType returnType =
  Python.lines
    [ "inputs = " + Python.FFI.argumentValue [(lhsArgName, lhsType), ("self", selfType)]
    , "output = " + Python.FFI.dummyValue returnType
    , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
    , "return " + Python.FFI.outputValue returnType "output"
    ]