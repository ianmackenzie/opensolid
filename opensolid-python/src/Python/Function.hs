module Python.Function
  ( overloadDeclaration
  , overloadCase
  , matchPattern
  , implicitValue
  , typePattern
  , arguments
  , argument
  , body
  )
where

import OpenSolid.API.ImplicitArgument (ImplicitArgument)
import OpenSolid.API.ImplicitArgument qualified as ImplicitArgument
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
  FFI.Result{} -> abort "Should never have Result as input argument"
  FFI.Class classId -> Python.Class.qualifiedName classId <> "()"

tuplePattern :: FFI.Type -> FFI.Type -> List FFI.Type -> Text
tuplePattern type1 type2 rest = do
  let itemPatterns = List.map typePattern (type1 : type2 : rest)
  "(" <> Text.join "," itemPatterns <> ")"

implicitValue :: ImplicitArgument -> (Text, FFI.Type)
implicitValue argType = (implicitGetter argType, ImplicitArgument.ffiType argType)

implicitGetter :: ImplicitArgument -> Text
implicitGetter argType = case argType of
  ImplicitArgument.ToleranceUnitless -> "_float_tolerance()"
  ImplicitArgument.ToleranceMeters -> "_length_tolerance()"
  ImplicitArgument.ToleranceSquareMeters -> "_area_tolerance()"
  ImplicitArgument.ToleranceRadians -> "_angle_tolerance()"

arguments :: "includeSelf" ::: Bool -> List (Name, FFI.Type) -> List (Name, FFI.Type) -> Text
arguments (Named includeSelf) positional named = do
  let selfArg = ["self" | includeSelf]
  let positionalArgs = List.map argument positional
  let separator = ["*" | not (List.isEmpty named)]
  let namedArgs = List.map argument named
  Text.join "," (List.concat [selfArg, positionalArgs, separator, namedArgs])

argument :: (Name, FFI.Type) -> Text
argument (argName, argType) = FFI.snakeCase argName <> ": " <> Python.Type.qualifiedName argType

body :: Text -> List (Text, FFI.Type) -> FFI.Type -> Text
body ffiFunctionName ffiArguments returnType =
  Python.lines
    [ "inputs = " <> Python.FFI.argumentValue ffiArguments
    , "output = " <> Python.FFI.dummyValue returnType
    , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
    , "return " <> Python.FFI.outputValue returnType "output"
    ]
