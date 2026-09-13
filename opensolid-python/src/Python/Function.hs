module Python.Function
  ( overloadDeclaration
  , overloadCase
  , matchPattern
  , implicitToleranceArgument
  , typePattern
  , arguments
  , argument
  , body
  )
where

import OpenSolid.API.ImplicitTolerance (ImplicitTolerance (ImplicitTolerance))
import OpenSolid.API.ImplicitTolerance qualified as ImplicitTolerance
import OpenSolid.FFI (Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude hiding (Type)
import OpenSolid.Text qualified as Text
import Python qualified
import Python.Class qualified
import Python.FFI qualified
import Python.Type qualified

overloadDeclaration :: Text -> Text
overloadDeclaration signature =
  Python.lines
    [ "@overload"
    , signature
    , Python.indent ["pass"]
    ]

overloadCase :: Text -> List Text -> Text
overloadCase givenMatchPattern caseBody =
  Python.lines
    [ "case " <> givenMatchPattern <> ":"
    , Python.indent caseBody
    ]

splits :: List a -> List (List a, List a)
splits [] = [([], [])]
splits list@(first : rest) = ([], list) : List.map (Pair.mapFirst (first :)) (splits rest)

singlePattern :: (List (Name, FFI.Type), List (Name, FFI.Type)) -> Text
singlePattern (positionalArguments, namedArguments) = do
  let positionalPattern = "[" <> Text.join "," (List.map asPattern positionalArguments) <> "]"
  let keywordPattern = "{" <> Text.join "," (List.map namedPattern namedArguments) <> "}"
  "(" <> positionalPattern <> ", " <> keywordPattern <> ")"

matchPattern :: List (Name, FFI.Type) -> Text
matchPattern givenArguments =
  Text.join " | " (List.map singlePattern (List.reverse (splits givenArguments)))

asPattern :: (Name, FFI.Type) -> Text
asPattern (argName, argType) = typePattern argType <> " as " <> FFI.snakeCase argName

namedPattern :: (Name, FFI.Type) -> Text
namedPattern (argName, argType) = do
  let name = FFI.snakeCase argName
  Python.str name <> ": " <> typePattern argType <> " as " <> name

typePattern :: FFI.Type -> Text
typePattern ffiType = case ffiType of
  FFI.Unit -> "None"
  FFI.Int -> "int()"
  FFI.Number -> "float() | int()"
  FFI.Bool -> "bool()"
  FFI.Sign -> "1 | -1"
  FFI.Text -> "str()"
  -- Note that there's no point trying to overload
  -- based on the type of items in the list, since it might be empty
  FFI.List{} -> "list()"
  -- For non-empty lists we _can_ overload
  -- based on the type of items in the list
  FFI.NonEmpty itemType -> "[" <> typePattern itemType <> ", *_]"
  -- Arrays are also non-empty
  FFI.Array itemType -> "[" <> typePattern itemType <> ", *_]"
  FFI.Tuple type1 type2 rest -> tuplePattern type1 type2 rest
  FFI.Maybe valueType -> typePattern valueType <> " | None"
  FFI.Class classId -> Python.Class.qualifiedName classId <> "()"

tuplePattern :: FFI.Type -> FFI.Type -> List FFI.Type -> Text
tuplePattern type1 type2 rest = do
  let itemPatterns = List.map typePattern (type1 : type2 : rest)
  "(" <> Text.join "," itemPatterns <> ")"

implicitToleranceArgument :: ImplicitTolerance -> (Text, FFI.Type)
implicitToleranceArgument ImplicitTolerance = ("_tolerance()", ImplicitTolerance.ffiType)

arguments :: "includeSelf" ::: Bool -> List (Name, FFI.Type) -> List (Name, FFI.Type) -> Text
arguments ("includeSelf" ::: includeSelf) positional named = do
  let selfArg = ["self" | includeSelf]
  let positionalArgs = List.map argument positional
  let separator = ["*" | not (List.isEmpty named)]
  let namedArgs = List.map argument named
  Text.join "," (List.concat [selfArg, positionalArgs, separator, namedArgs])

argument :: (Name, FFI.Type) -> Text
argument (argName, argType) = FFI.snakeCase argName <> ": " <> Python.Type.qualifiedName argType

return :: FFI.Type -> Text -> Text
return FFI.Unit _ = "return"
return ffiType varName = "return " <> Python.FFI.outputValue ffiType varName

body :: Text -> List (Text, FFI.Type) -> FFI.Type -> Text
body ffiFunctionName ffiArguments returnType = do
  let argumentsName = ffiFunctionName <> "_arguments"
  let outputName = ffiFunctionName <> "_output"
  let statusName = ffiFunctionName <> "_status"
  let errorMessageName = ffiFunctionName <> "_error_message"
  Python.lines
    [ argumentsName <> " = " <> Python.FFI.argumentValue ffiArguments
    , outputName <> " = " <> Python.FFI.dummyValue returnType
    , errorMessageName <> " = " <> Python.FFI.dummyValue FFI.Text
    , Text.sentence
        [ statusName
        , "="
        , Python.FFI.invoke
            ffiFunctionName
            ("ctypes.byref(" <> argumentsName <> ")")
            ("ctypes.byref(" <> outputName <> ")")
            ("ctypes.byref(" <> errorMessageName <> ")")
        ]
    , "if " <> statusName <> " == 0:"
    , "    " <> return returnType outputName
    , "else:"
    , "    _error(_text_to_str(" <> errorMessageName <> "))"
    ]
