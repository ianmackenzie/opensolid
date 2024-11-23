module OpenSolid.API.Class.Constructor
  ( Constructor (..)
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
import Tolerance qualified

data Constructor value where
  C0 ::
    FFI value =>
    Constraint constraint ->
    (constraint => value) ->
    Constructor value
  C1 ::
    (FFI a, FFI value) =>
    Constraint constraint ->
    Text ->
    (constraint => a -> value) ->
    Constructor value
  C2 ::
    (FFI a, FFI b, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    (constraint => a -> b -> value) ->
    Constructor value
  C3 ::
    (FFI a, FFI b, FFI c, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> value) ->
    Constructor value
  C4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value) =>
    Constraint constraint ->
    Text ->
    Text ->
    Text ->
    Text ->
    (constraint => a -> b -> c -> d -> value) ->
    Constructor value

ffiName :: Constructor value -> Text
ffiName constructor = case constructor of
  C0 N _ -> ffiName0
  C0 F _ -> ffiName0
  C0 L _ -> ffiName0
  C1 N _ f -> ffiName1 f
  C1 F _ f -> ffiName1 (Tolerance.exactly f)
  C1 L _ f -> ffiName1 (Tolerance.exactly f)
  C2 N _ _ f -> ffiName2 f
  C2 F _ _ f -> ffiName2 (Tolerance.exactly f)
  C2 L _ _ f -> ffiName2 (Tolerance.exactly f)
  C3 N _ _ _ f -> ffiName3 f
  C3 F _ _ _ f -> ffiName3 (Tolerance.exactly f)
  C3 L _ _ _ f -> ffiName3 (Tolerance.exactly f)
  C4 N _ _ _ _ f -> ffiName4 f
  C4 F _ _ _ _ f -> ffiName4 (Tolerance.exactly f)
  C4 L _ _ _ _ f -> ffiName4 (Tolerance.exactly f)

ffiName0 :: Text
ffiName0 = "constructor"

ffiName1 :: forall a value. (FFI a, FFI value) => (a -> value) -> Text
ffiName1 _ = "constructor__" + FFI.compositeName [FFI.typeName @a Proxy]

ffiName2 :: forall a b value. (FFI a, FFI b, FFI value) => (a -> b -> value) -> Text
ffiName2 _ = "constructor__" + FFI.compositeName [FFI.typeName @a Proxy, FFI.typeName @b Proxy]

ffiName3 ::
  forall a b c value.
  (FFI a, FFI b, FFI c, FFI value) =>
  (a -> b -> c -> value) ->
  Text
ffiName3 _ =
  "constructor__"
    + FFI.compositeName
      [ FFI.typeName @a Proxy
      , FFI.typeName @b Proxy
      , FFI.typeName @c Proxy
      ]

ffiName4 ::
  forall a b c d value.
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  (a -> b -> c -> d -> value) ->
  Text
ffiName4 _ =
  "constructor__"
    + FFI.compositeName
      [ FFI.typeName @a Proxy
      , FFI.typeName @b Proxy
      , FFI.typeName @c Proxy
      , FFI.typeName @d Proxy
      ]

invoke :: Constructor value -> Ptr () -> Ptr () -> IO ()
invoke constructor = case constructor of
  C0 N v ->
    \_ outputPtr -> FFI.store outputPtr 0 v
  C0 F v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  C0 L v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  C1 N _ f ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  C1 F _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  C1 L _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  C2 N _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  C2 F _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  C2 L _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  C3 N _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  C3 F _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  C3 L _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  C4 N _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)
  C4 F _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  C4 L _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
