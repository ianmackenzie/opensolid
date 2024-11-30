module Python.MemberFunction (definition) where

import List qualified
import Maybe qualified
import OpenSolid
import OpenSolid.API.Constraint (Constraint)
import OpenSolid.API.MemberFunction (MemberFunction (..))
import OpenSolid.API.MemberFunction qualified as MemberFunction
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Python qualified
import Python.FFI qualified
import Python.Function qualified
import Python.Type qualified
import Text qualified

definition :: Text -> (Name, List (MemberFunction value)) -> Text
definition functionPrefix (functionName, memberFunctions) = do
  case List.map (overload functionPrefix functionName) memberFunctions of
    [(signature, _, body)] -> Python.lines [signature, Python.indent [body]]
    overloads -> do
      let overloadDeclaration (signature, _, _) = Python.Function.overloadDeclaration signature
      let overloadCase (_, matchPattern, body) = Python.Function.overloadCase matchPattern [body]
      Python.lines
        [ Python.lines (List.map overloadDeclaration overloads)
        , "def " + Name.snakeCase functionName + "(self, *args, **keywords):"
        , Python.indent
            [ "match (args, keywords):"
            , Python.indent
                [ Python.lines (List.map overloadCase overloads)
                , "case _:"
                , Python.indent
                    [ "message = \"Unexpected function arguments\""
                    , "raise TypeError(message)"
                    ]
                ]
            ]
        ]

overload :: Text -> Name -> MemberFunction value -> (Text, Text, Text)
overload functionPrefix functionName memberFunction = do
  let ffiFunctionName = functionPrefix + MemberFunction.ffiName functionName memberFunction
  let (maybeConstraint, arguments, selfType, returnType) = MemberFunction.signature memberFunction
  let signature = signatureN functionName arguments returnType
  let matchPattern = Python.Function.matchPattern arguments
  let body = bodyN ffiFunctionName maybeConstraint arguments selfType returnType
  (signature, matchPattern, body)

signatureN :: Name -> List (Name, FFI.Type) -> FFI.Type -> Text
signatureN functionName args returnType = do
  let functionArgument (argName, argType) = Name.snakeCase argName + ": " + Python.Type.name argType
  let functionArguments = Text.join "," ("self" : List.map functionArgument args)
  "def " + Name.snakeCase functionName + "(" + functionArguments + ") -> " + Python.Type.name returnType + ":"

bodyN :: Text -> Maybe Constraint -> List (Name, FFI.Type) -> FFI.Type -> FFI.Type -> Text
bodyN ffiFunctionName maybeConstraint arguments selfType returnType = do
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments = List.map (Pair.mapFirst Name.snakeCase) arguments
  let selfArgument = ("self", selfType)
  Python.lines
    [ "inputs = " + Python.FFI.argumentValue (maybeToleranceArgument + normalArguments + [selfArgument])
    , "output = " + Python.FFI.dummyValue returnType
    , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
    , "return " + Python.FFI.outputValue returnType "output"
    ]
