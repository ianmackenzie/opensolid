module OpenSolid.API.Class.MemberFunction
  ( MemberFunction (..)
  , ffiName
  , invoke
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import IO qualified
import OpenSolid
import OpenSolid.API.Constraint (Constraint (..))
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Text qualified
import Tolerance qualified

data MemberFunction value where
  M0 ::
    (FFI value, FFI result) =>
    Constraint constraint ->
    (constraint => value -> result) ->
    MemberFunction value
  M1 ::
    (FFI a, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    (constraint => a -> value -> result) ->
    MemberFunction value
  M2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    (constraint => a -> b -> value -> result) ->
    MemberFunction value
  M3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> value -> result) ->
    MemberFunction value
  M4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d -> value -> result) ->
    MemberFunction value

toSnakeCase :: Text -> Text
toSnakeCase = Text.replace " " "_"

ffiName :: Text -> MemberFunction value -> Text
ffiName functionName memberFunction = case memberFunction of
  M0 N _ -> toSnakeCase functionName
  M0 F _ -> toSnakeCase functionName
  M0 L _ -> toSnakeCase functionName
  M1 N _ f -> ffiName1 functionName f
  M1 F _ f -> ffiName1 functionName (Tolerance.exactly f)
  M1 L _ f -> ffiName1 functionName (Tolerance.exactly f)
  M2 N _ _ f -> ffiName2 functionName f
  M2 F _ _ f -> ffiName2 functionName (Tolerance.exactly f)
  M2 L _ _ f -> ffiName2 functionName (Tolerance.exactly f)
  M3 N _ _ _ f -> ffiName3 functionName f
  M3 F _ _ _ f -> ffiName3 functionName (Tolerance.exactly f)
  M3 L _ _ _ f -> ffiName3 functionName (Tolerance.exactly f)
  M4 N _ _ _ _ f -> ffiName4 functionName f
  M4 F _ _ _ _ f -> ffiName4 functionName (Tolerance.exactly f)
  M4 L _ _ _ _ f -> ffiName4 functionName (Tolerance.exactly f)

ffiName1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Text ->
  (a -> value -> result) ->
  Text
ffiName1 functionName _ =
  toSnakeCase functionName + "__" + FFI.typeName @a Proxy

ffiName2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Text ->
  (a -> b -> value -> result) ->
  Text
ffiName2 functionName _ =
  toSnakeCase functionName
    + "__"
    + FFI.compositeName
      [ FFI.typeName @a Proxy
      , FFI.typeName @b Proxy
      ]

ffiName3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Text ->
  (a -> b -> c -> value -> result) ->
  Text
ffiName3 functionName _ =
  toSnakeCase functionName
    + "__"
    + FFI.compositeName
      [ FFI.typeName @a Proxy
      , FFI.typeName @b Proxy
      , FFI.typeName @c Proxy
      ]

ffiName4 ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Text ->
  (a -> b -> c -> d -> value -> result) ->
  Text
ffiName4 functionName _ =
  toSnakeCase functionName
    + "__"
    + FFI.compositeName
      [ FFI.typeName @a Proxy
      , FFI.typeName @b Proxy
      , FFI.typeName @c Proxy
      , FFI.typeName @d Proxy
      ]

invoke :: MemberFunction value -> Ptr () -> Ptr () -> IO ()
invoke function = case function of
  M0 N f ->
    \inputPtr outputPtr -> IO.do
      self <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f self)
  M0 F f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  M0 L f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  M1 N _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 self)
  M1 F _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  M1 L _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  M2 N _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 self)
  M2 F _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  M2 L _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  M3 N _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 self)
  M3 F _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  M3 L _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  M4 N _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4 self)
  M4 F _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
  M4 L _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
