module Function
  ( overloadDeclaration
  , overloadCase
  , matchPattern0
  , matchPattern1
  , matchPattern2
  , matchPattern3
  , matchPattern4
  )
where

import Class qualified
import Data.Proxy (Proxy (Proxy))
import OpenSolid
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Python qualified
import Text qualified

overloadDeclaration :: Text -> Text
overloadDeclaration signature =
  Python.lines
    [ "@overload"
    , signature
    , "    pass"
    ]

overloadCase :: Text -> List Text -> Text
overloadCase matchPattern body =
  Python.lines
    [ "case " + matchPattern + ":"
    , Python.indent body
    ]

asPattern :: forall a. FFI a => Proxy a -> Name -> Text
asPattern _ argName = typePattern @a Proxy + " as " + Name.snakeCase argName

namedPattern :: forall a. FFI a => Proxy a -> Name -> Text
namedPattern _ argName =
  Python.str (Name.snakeCase argName)
    + ": "
    + typePattern @a Proxy
    + " as "
    + Name.snakeCase argName

matchPattern0 :: Text
matchPattern0 = "([], entries) if not entries"

matchPattern1 :: forall a value. FFI a => Name -> (a -> value) -> Text
matchPattern1 argName1 _ = do
  let asPattern1 = asPattern @a Proxy argName1
  let positionalPattern = "([" + asPattern1 + "],{})"
  let namedPattern1 = namedPattern @a Proxy argName1
  let keywordPattern = "([],{" + namedPattern1 + "})"
  positionalPattern + " | " + keywordPattern

matchPattern2 ::
  forall a b value.
  (FFI a, FFI b) =>
  Name ->
  Name ->
  (a -> b -> value) ->
  Text
matchPattern2 argName1 argName2 _ = do
  let asPattern1 = asPattern @a Proxy argName1
  let asPattern2 = asPattern @b Proxy argName2
  let positionalPattern = "([" + asPattern1 + "," + asPattern2 + "],{})"
  let namedPattern1 = namedPattern @a Proxy argName1
  let namedPattern2 = namedPattern @b Proxy argName2
  let keywordPattern = "([],{" + namedPattern1 + "," + namedPattern2 + "})"
  positionalPattern + " | " + keywordPattern

matchPattern3 ::
  forall a b c value.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> value) ->
  Text
matchPattern3 argName1 argName2 argName3 _ = do
  let asPattern1 = asPattern @a Proxy argName1
  let asPattern2 = asPattern @b Proxy argName2
  let asPattern3 = asPattern @c Proxy argName3
  let positionalPattern = "([" + asPattern1 + "," + asPattern2 + "," + asPattern3 + "],{})"
  let namedPattern1 = namedPattern @a Proxy argName1
  let namedPattern2 = namedPattern @b Proxy argName2
  let namedPattern3 = namedPattern @c Proxy argName3
  let keywordPattern = "([], {" + namedPattern1 + "," + namedPattern2 + "," + namedPattern3 + "})"
  positionalPattern + " | " + keywordPattern

matchPattern4 ::
  forall a b c d value.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> value) ->
  Text
matchPattern4 argName1 argName2 argName3 argName4 _ = do
  let asPattern1 = asPattern @a Proxy argName1
  let asPattern2 = asPattern @b Proxy argName2
  let asPattern3 = asPattern @c Proxy argName3
  let asPattern4 = asPattern @d Proxy argName4
  let positionalPattern = "([" + asPattern1 + "," + asPattern2 + "," + asPattern3 + "," + asPattern4 + "],{})"
  let namedPattern1 = namedPattern @a Proxy argName1
  let namedPattern2 = namedPattern @b Proxy argName2
  let namedPattern3 = namedPattern @c Proxy argName3
  let namedPattern4 = namedPattern @d Proxy argName4
  let keywordPattern = "([],{" + namedPattern1 + "," + namedPattern2 + "," + namedPattern3 + "," + namedPattern4 + "})"
  positionalPattern + " | " + keywordPattern

typePattern :: FFI a => Proxy a -> Text
typePattern proxy = case FFI.representation proxy of
  FFI.Int -> "int()"
  FFI.Float -> "float() | int()"
  FFI.Qty className -> className + "()"
  -- Note that there's no point trying to overload
  -- based on the type of items in the list,
  -- since it might be empty
  FFI.List -> "list()"
  FFI.Tuple2 -> tuple2Pattern proxy
  FFI.Tuple3 -> tuple3Pattern proxy
  FFI.Tuple4 -> tuple4Pattern proxy
  FFI.Tuple5 -> tuple5Pattern proxy
  FFI.Tuple6 -> tuple6Pattern proxy
  FFI.Maybe -> maybePattern proxy
  FFI.Result -> internalError "Should never have Result as input argument"
  FFI.Class baseName maybeUnits -> Class.name baseName maybeUnits + "()"

tuplePattern :: List Text -> Text
tuplePattern itemPatterns = "(" + Text.join "," itemPatterns + ")"

tuple2Pattern :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Text
tuple2Pattern _ =
  tuplePattern [typePattern @a Proxy, typePattern @b Proxy]

tuple3Pattern :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> Text
tuple3Pattern _ =
  tuplePattern [typePattern @a Proxy, typePattern @b Proxy, typePattern @c Proxy]

tuple4Pattern :: forall a b c d. (FFI a, FFI b, FFI c, FFI d) => Proxy (a, b, c, d) -> Text
tuple4Pattern _ =
  tuplePattern
    [ typePattern @a Proxy
    , typePattern @b Proxy
    , typePattern @c Proxy
    , typePattern @d Proxy
    ]

tuple5Pattern :: forall a b c d e. (FFI a, FFI b, FFI c, FFI d, FFI e) => Proxy (a, b, c, d, e) -> Text
tuple5Pattern _ =
  tuplePattern
    [ typePattern @a Proxy
    , typePattern @b Proxy
    , typePattern @c Proxy
    , typePattern @d Proxy
    , typePattern @e Proxy
    ]

tuple6Pattern :: forall a b c d e f. (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) => Proxy (a, b, c, d, e, f) -> Text
tuple6Pattern _ =
  tuplePattern
    [ typePattern @a Proxy
    , typePattern @b Proxy
    , typePattern @c Proxy
    , typePattern @d Proxy
    , typePattern @e Proxy
    , typePattern @f Proxy
    ]

maybePattern :: forall a. FFI a => Proxy (Maybe a) -> Text
maybePattern _ = typePattern @a Proxy + " | None"
