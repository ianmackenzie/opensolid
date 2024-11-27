module OpenSolid.API.StaticFunction
  ( StaticFunction (..)
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

data StaticFunction where
  S0 ::
    FFI a =>
    Constraint constraint ->
    (constraint => a) ->
    StaticFunction
  S1 ::
    (FFI a, FFI b) =>
    Constraint constraint ->
    Text ->
    (constraint => a -> b) ->
    StaticFunction
  S2 ::
    (FFI a, FFI b, FFI c) =>
    Constraint constraint ->
    Text ->
    Text ->
    (constraint => a -> b -> c) ->
    StaticFunction
  S3 ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d) ->
    StaticFunction
  S4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d -> e) ->
    StaticFunction

ffiName :: Text -> StaticFunction -> Text
ffiName functionName staticFunction = case staticFunction of
  S0 N _ -> FFI.toCamelCase functionName
  S0 F _ -> FFI.toCamelCase functionName
  S0 L _ -> FFI.toCamelCase functionName
  S1 N _ f -> ffiName1 functionName f
  S1 F _ f -> ffiName1 functionName (Tolerance.exactly f)
  S1 L _ f -> ffiName1 functionName (Tolerance.exactly f)
  S2 N _ _ f -> ffiName2 functionName f
  S2 F _ _ f -> ffiName2 functionName (Tolerance.exactly f)
  S2 L _ _ f -> ffiName2 functionName (Tolerance.exactly f)
  S3 N _ _ _ f -> ffiName3 functionName f
  S3 F _ _ _ f -> ffiName3 functionName (Tolerance.exactly f)
  S3 L _ _ _ f -> ffiName3 functionName (Tolerance.exactly f)
  S4 N _ _ _ _ f -> ffiName4 functionName f
  S4 F _ _ _ _ f -> ffiName4 functionName (Tolerance.exactly f)
  S4 L _ _ _ _ f -> ffiName4 functionName (Tolerance.exactly f)

ffiName1 :: forall a value. (FFI a, FFI value) => Text -> (a -> value) -> Text
ffiName1 functionName _ =
  FFI.toCamelCase functionName + "_" + FFI.typeName @a Proxy

ffiName2 :: forall a b value. (FFI a, FFI b, FFI value) => Text -> (a -> b -> value) -> Text
ffiName2 functionName _ =
  Text.join "_" $
    [ FFI.toCamelCase functionName
    , FFI.typeName @a Proxy
    , FFI.typeName @b Proxy
    ]

ffiName3 ::
  forall a b c value.
  (FFI a, FFI b, FFI c, FFI value) =>
  Text ->
  (a -> b -> c -> value) ->
  Text
ffiName3 functionName _ =
  Text.join "_" $
    [ FFI.toCamelCase functionName
    , FFI.typeName @a Proxy
    , FFI.typeName @b Proxy
    , FFI.typeName @c Proxy
    ]

ffiName4 ::
  forall a b c d value.
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Text ->
  (a -> b -> c -> d -> value) ->
  Text
ffiName4 functionName _ =
  Text.join "_" $
    [ FFI.toCamelCase functionName
    , FFI.typeName @a Proxy
    , FFI.typeName @b Proxy
    , FFI.typeName @c Proxy
    , FFI.typeName @d Proxy
    ]

invoke :: StaticFunction -> Ptr () -> Ptr () -> IO ()
invoke function = case function of
  S0 N v ->
    \_ outputPtr -> FFI.store outputPtr 0 v
  S0 F v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  S0 L v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  S1 N _ f ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  S1 F _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  S1 L _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  S2 N _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  S2 F _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  S2 L _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  S3 N _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  S3 F _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  S3 L _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  S4 N _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)
  S4 F _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  S4 L _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
