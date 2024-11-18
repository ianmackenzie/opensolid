module Function
  ( name
  , overloadDeclaration
  , overloadCase
  , matchPattern0
  , matchPattern1
  , matchPattern2
  , matchPattern3
  , matchPattern4
  )
where

import Data.Proxy (Proxy (Proxy))
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Python qualified
import Text qualified

name :: Text -> Text
name = Text.replace " " "_"

overloadDeclaration :: Text -> Text
overloadDeclaration signature =
  Python.lines
    [ "@overload"
    , signature
    , "    pass"
    ]

overloadCase :: Text -> List Text -> Text
overloadCase pattern body =
  Python.lines
    [ "case " + pattern + ":"
    , Python.indent body
    ]

asPattern :: forall a. FFI a => Proxy a -> Text -> Text
asPattern _ varName = typePattern @a Proxy + " as " + varName

namedPattern :: forall a. FFI a => Proxy a -> Text -> Text
namedPattern _ argName =
  Python.str argName + ": " + typePattern @a Proxy + " as " + argName

matchPattern0 :: Text
matchPattern0 = "([], {**entries}) if not entries"

matchPattern1 :: forall a value. FFI a => Text -> (a -> value) -> Text
matchPattern1 argName1 _ = do
  let asPattern1 = asPattern @a Proxy argName1
  let positionalPattern = "([" + asPattern1 + "],{**rest})"
  let namedPattern1 = namedPattern @a Proxy argName1
  let keywordPattern = "([],{" + namedPattern1 + ", **rest})"
  "(" + positionalPattern + " | " + keywordPattern + ") if not rest"

matchPattern2 ::
  forall a b value.
  (FFI a, FFI b) =>
  Text ->
  Text ->
  (a -> b -> value) ->
  Text
matchPattern2 argName1 argName2 _ = do
  let asPattern1 = asPattern @a Proxy argName1
  let asPattern2 = asPattern @b Proxy argName2
  let positionalPattern = "([" + asPattern1 + "," + asPattern2 + "],{**rest})"
  let namedPattern1 = namedPattern @a Proxy argName1
  let namedPattern2 = namedPattern @b Proxy argName2
  let keywordPattern = "([],{" + namedPattern1 + "," + namedPattern2 + ", **rest})"
  "(" + positionalPattern + " | " + keywordPattern + ") if not rest"

matchPattern3 ::
  forall a b c value.
  (FFI a, FFI b, FFI c) =>
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> value) ->
  Text
matchPattern3 argName1 argName2 argName3 _ = do
  let asPattern1 = asPattern @a Proxy argName1
  let asPattern2 = asPattern @b Proxy argName2
  let asPattern3 = asPattern @c Proxy argName3
  let positionalPattern = "([" + asPattern1 + "," + asPattern2 + "," + asPattern3 + "],{**rest})"
  let namedPattern1 = namedPattern @a Proxy argName1
  let namedPattern2 = namedPattern @b Proxy argName2
  let namedPattern3 = namedPattern @c Proxy argName3
  let keywordPattern = "([], {" + namedPattern1 + "," + namedPattern2 + "," + namedPattern3 + ", **rest})"
  "(" + positionalPattern + " | " + keywordPattern + ") if not rest"

matchPattern4 ::
  forall a b c d value.
  (FFI a, FFI b, FFI c, FFI d) =>
  Text ->
  Text ->
  Text ->
  Text ->
  (a -> b -> c -> d -> value) ->
  Text
matchPattern4 argName1 argName2 argName3 argName4 _ = do
  let asPattern1 = asPattern @a Proxy argName1
  let asPattern2 = asPattern @b Proxy argName2
  let asPattern3 = asPattern @c Proxy argName3
  let asPattern4 = asPattern @d Proxy argName4
  let positionalPattern = "([" + asPattern1 + "," + asPattern2 + "," + asPattern3 + "," + asPattern4 + "],{**rest})"
  let namedPattern1 = namedPattern @a Proxy argName1
  let namedPattern2 = namedPattern @b Proxy argName2
  let namedPattern3 = namedPattern @c Proxy argName3
  let namedPattern4 = namedPattern @d Proxy argName4
  let keywordPattern = "([],{" + namedPattern1 + "," + namedPattern2 + "," + namedPattern3 + "," + namedPattern4 + ", **rest})"
  "(" + positionalPattern + " | " + keywordPattern + ") if not rest"

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
  FFI.Class className -> className + "()"

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
