module Python.Function
  ( overloadDeclaration
  , overloadCase
  , matchPattern
  , toleranceArgument
  , typePattern
  , arguments
  , argument
  , body
  )
where

import API.Constraint (Constraint)
import API.Constraint qualified as Constraint
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
  FFI.Float -> "float() | int()"
  FFI.Bool -> "bool()"
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
  FFI.Result{} -> internalError "Should never have Result as input argument"
  FFI.Class id -> Python.Class.qualifiedName id <> "()"

tuplePattern :: FFI.Type -> FFI.Type -> List FFI.Type -> Text
tuplePattern type1 type2 rest = do
  let itemPatterns = List.map typePattern (type1 : type2 : rest)
  "(" <> Text.join "," itemPatterns <> ")"

toleranceArgument :: Constraint -> (Text, FFI.Type)
toleranceArgument constraint = (toleranceFunction constraint, Constraint.toleranceType constraint)

toleranceFunction :: Constraint -> Text
toleranceFunction constraint = case constraint of
  Constraint.ToleranceUnitless -> "_float_tolerance()"
  Constraint.ToleranceMeters -> "_length_tolerance()"
  Constraint.ToleranceSquareMeters -> "_area_tolerance()"
  Constraint.ToleranceRadians -> "_angle_tolerance()"

arguments :: Bool -> List (Name, FFI.Type) -> List (Name, FFI.Type) -> Text
arguments includeSelf positional named = do
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
