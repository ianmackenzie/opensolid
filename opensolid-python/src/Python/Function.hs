module Python.Function
  ( overloadDeclaration
  , overloadCase
  , matchPattern
  , toleranceArgument
  , typePattern
  )
where

import List qualified
import OpenSolid hiding (Type)
import OpenSolid.API.Constraint (Constraint)
import OpenSolid.API.Constraint qualified as Constraint
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Python qualified
import Python.Class qualified
import Text qualified

overloadDeclaration :: Text -> Text
overloadDeclaration signature =
  Python.lines
    [ "@overload"
    , signature
    , Python.indent ["pass"]
    ]

overloadCase :: Text -> List Text -> Text
overloadCase givenMatchPattern body =
  Python.lines
    [ "case " + givenMatchPattern + ":"
    , Python.indent body
    ]

splits :: List a -> List (List a, List a)
splits [] = [([], [])]
splits list@(first : rest) = ([], list) : List.map (Pair.mapFirst (first :)) (splits rest)

singlePattern :: (List (Name, FFI.Type), List (Name, FFI.Type)) -> Text
singlePattern (positionalArguments, namedArguments) = do
  let positionalPattern = "[" + Text.join "," (List.map asPattern positionalArguments) + "]"
  let keywordPattern = "{" + Text.join "," (List.map namedPattern namedArguments) + "}"
  "(" + positionalPattern + ", " + keywordPattern + ")"

matchPattern :: List (Name, FFI.Type) -> Text
matchPattern arguments =
  Text.join " | " (List.map singlePattern (List.reverse (splits arguments)))

asPattern :: (Name, FFI.Type) -> Text
asPattern (argName, argType) = typePattern argType + " as " + Name.snakeCase argName

namedPattern :: (Name, FFI.Type) -> Text
namedPattern (argName, argType) = do
  let name = Name.snakeCase argName
  Python.str name + ": " + typePattern argType + " as " + name

typePattern :: FFI.Type -> Text
typePattern ffiType = case ffiType of
  FFI.Int -> "int()"
  FFI.Float -> "float() | int()"
  FFI.Bool -> "bool()"
  FFI.Text -> "str()"
  -- Note that there's no point trying to overload
  -- based on the type of items in the list, since it might be empty
  FFI.List{} -> "list()"
  -- For non-empty lists we _can_ overload
  -- based on the type of items in the list
  FFI.NonEmpty itemType -> "[" + typePattern itemType + ", *_]"
  FFI.Tuple type1 type2 rest -> tuplePattern type1 type2 rest
  FFI.Maybe valueType -> typePattern valueType + " | None"
  FFI.Result{} -> internalError "Should never have Result as input argument"
  FFI.Class id -> Python.Class.qualifiedName id + "()"

tuplePattern :: FFI.Type -> FFI.Type -> List FFI.Type -> Text
tuplePattern type1 type2 rest = do
  let itemPatterns = List.map typePattern (type1 : type2 : rest)
  "(" + Text.join "," itemPatterns + ")"

toleranceArgument :: Constraint -> (Text, FFI.Type)
toleranceArgument constraint = (toleranceFunction constraint, Constraint.toleranceType constraint)

toleranceFunction :: Constraint -> Text
toleranceFunction constraint = case constraint of
  Constraint.ToleranceUnitless -> "_float_tolerance()"
  Constraint.ToleranceMeters -> "_length_tolerance()"
  Constraint.ToleranceRadians -> "_angle_tolerance()"
