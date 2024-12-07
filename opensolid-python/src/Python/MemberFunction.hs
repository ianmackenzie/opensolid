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

argumentCount :: MemberFunction value -> Int
argumentCount staticFunction = do
  let (_, arguments, _, _) = MemberFunction.signature staticFunction
  List.length arguments

definition :: FFI.Id value -> (Name, List (MemberFunction value)) -> Text
definition classId (functionName, memberFunctions) = do
  let sortedFunctions = List.reverse (List.sortBy argumentCount memberFunctions)
  case List.map (overload classId functionName) sortedFunctions of
    [(signature, _, body)] -> Python.lines [signature, Python.indent [body]]
    overloads -> do
      let overloadDeclaration (signature, _, _) = Python.Function.overloadDeclaration signature
      let overloadCase (_, matchPattern, body) = Python.Function.overloadCase matchPattern [body]
      Python.lines
        [ Python.lines (List.map overloadDeclaration overloads)
        , "def " + toPython functionName + "(self, *args, **keywords):"
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

toPython :: Name -> Text
toPython functionName =
  case Name.snakeCase functionName of
    "contains" -> "__contains__"
    other -> other

overload :: FFI.Id value -> Name -> MemberFunction value -> (Text, Text, Text)
overload classId functionName memberFunction = do
  let ffiFunctionName = MemberFunction.ffiName classId functionName memberFunction
  let (maybeConstraint, arguments, selfType, returnType) = MemberFunction.signature memberFunction
  let signature = overloadSignature functionName arguments returnType
  let matchPattern = Python.Function.matchPattern arguments
  let body = overloadBody ffiFunctionName maybeConstraint arguments selfType returnType
  (signature, matchPattern, body)

overloadSignature :: Name -> List (Name, FFI.Type) -> FFI.Type -> Text
overloadSignature functionName args returnType = do
  let functionArgument (argName, argType) = Name.snakeCase argName + ": " + Python.Type.qualifiedName argType
  let functionArguments = Text.join "," ("self" : List.map functionArgument args)
  "def " + toPython functionName + "(" + functionArguments + ") -> " + Python.Type.qualifiedName returnType + ":"

overloadBody :: Text -> Maybe Constraint -> List (Name, FFI.Type) -> FFI.Type -> FFI.Type -> Text
overloadBody ffiFunctionName maybeConstraint arguments selfType returnType = do
  let maybeToleranceArgument = Maybe.map Python.Function.toleranceArgument maybeConstraint
  let normalArguments = List.map (Pair.mapFirst Name.snakeCase) arguments
  let selfArgument = ("self", selfType)
  Python.lines
    [ "inputs = " + Python.FFI.argumentValue (maybeToleranceArgument + normalArguments + [selfArgument])
    , "output = " + Python.FFI.dummyValue returnType
    , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
    , "return " + Python.FFI.outputValue returnType "output"
    ]
