module Type (name) where

import Class qualified
import Data.Proxy (Proxy (Proxy))
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Python qualified

name :: forall a. FFI a => Proxy a -> Text
name proxy = case FFI.representation proxy of
  FFI.Int -> "int"
  FFI.Float -> "float"
  FFI.Qty className -> className
  FFI.List -> listName proxy
  FFI.Tuple2 -> tuple2Name proxy
  FFI.Tuple3 -> tuple3Name proxy
  FFI.Tuple4 -> tuple4Name proxy
  FFI.Tuple5 -> tuple5Name proxy
  FFI.Tuple6 -> tuple6Name proxy
  FFI.Maybe -> maybeName proxy
  FFI.Result -> resultName proxy
  FFI.Class baseName maybeUnits -> Class.name baseName maybeUnits

listName :: forall a. FFI a => Proxy (List a) -> Text
listName _ = "list[" + name @a Proxy + "]"

tupleName :: List Text -> Text
tupleName names = "tuple" + Python.list names

tuple2Name :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Text
tuple2Name _ = tupleName [name @a Proxy, name @b Proxy]

tuple3Name :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> Text
tuple3Name _ = tupleName [name @a Proxy, name @b Proxy, name @c Proxy]

tuple4Name ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Proxy (a, b, c, d) ->
  Text
tuple4Name _ =
  tupleName
    [ name @a Proxy
    , name @b Proxy
    , name @c Proxy
    , name @d Proxy
    ]

tuple5Name ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Proxy (a, b, c, d, e) ->
  Text
tuple5Name _ =
  tupleName
    [ name @a Proxy
    , name @b Proxy
    , name @c Proxy
    , name @d Proxy
    , name @e Proxy
    ]

tuple6Name ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Proxy (a, b, c, d, e, f) ->
  Text
tuple6Name _ =
  tupleName
    [ name @a Proxy
    , name @b Proxy
    , name @c Proxy
    , name @d Proxy
    , name @e Proxy
    , name @f Proxy
    ]

maybeName :: forall a. FFI a => Proxy (Maybe a) -> Text
maybeName _ = name @a Proxy + "| None"

resultName :: forall x a. FFI a => Proxy (Result x a) -> Text
resultName _ = name @a Proxy
