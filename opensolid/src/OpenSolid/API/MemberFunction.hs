module OpenSolid.API.MemberFunction
  ( MemberFunction (..)
  , ffiName
  , invoke
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import IO qualified
import OpenSolid
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Text qualified
import Tolerance qualified
import Units (Meters)

data MemberFunction value where
  MemberFunction0 ::
    (FFI value, FFI result) =>
    (value -> result) ->
    MemberFunction value
  MemberFunction0U ::
    (FFI value, FFI result) =>
    (Tolerance Unitless => value -> result) ->
    MemberFunction value
  MemberFunction0M ::
    (FFI value, FFI result) =>
    (Tolerance Meters => value -> result) ->
    MemberFunction value
  MemberFunction1 ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (a -> value -> result) ->
    MemberFunction value
  MemberFunction1U ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (Tolerance Unitless => a -> value -> result) ->
    MemberFunction value
  MemberFunction1M ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (Tolerance Meters => a -> value -> result) ->
    MemberFunction value
  MemberFunction2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (a -> b -> value -> result) ->
    MemberFunction value
  MemberFunction2U ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> value -> result) ->
    MemberFunction value
  MemberFunction2M ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> value -> result) ->
    MemberFunction value
  MemberFunction3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> value -> result) ->
    MemberFunction value
  MemberFunction3U ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c -> value -> result) ->
    MemberFunction value
  MemberFunction3M ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> value -> result) ->
    MemberFunction value
  MemberFunction4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> value -> result) ->
    MemberFunction value
  MemberFunction4U ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c -> d -> value -> result) ->
    MemberFunction value
  MemberFunction4M ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> value -> result) ->
    MemberFunction value

ffiName :: Name -> MemberFunction value -> Text
ffiName functionName memberFunction = case memberFunction of
  MemberFunction0 _ -> Name.camelCase functionName
  MemberFunction0U _ -> Name.camelCase functionName
  MemberFunction0M _ -> Name.camelCase functionName
  MemberFunction1 _ f -> ffiName1 functionName f
  MemberFunction1U _ f -> ffiName1 functionName (Tolerance.exactly f)
  MemberFunction1M _ f -> ffiName1 functionName (Tolerance.exactly f)
  MemberFunction2 _ _ f -> ffiName2 functionName f
  MemberFunction2U _ _ f -> ffiName2 functionName (Tolerance.exactly f)
  MemberFunction2M _ _ f -> ffiName2 functionName (Tolerance.exactly f)
  MemberFunction3 _ _ _ f -> ffiName3 functionName f
  MemberFunction3U _ _ _ f -> ffiName3 functionName (Tolerance.exactly f)
  MemberFunction3M _ _ _ f -> ffiName3 functionName (Tolerance.exactly f)
  MemberFunction4 _ _ _ _ f -> ffiName4 functionName f
  MemberFunction4U _ _ _ _ f -> ffiName4 functionName (Tolerance.exactly f)
  MemberFunction4M _ _ _ _ f -> ffiName4 functionName (Tolerance.exactly f)

ffiName1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (a -> value -> result) ->
  Text
ffiName1 functionName _ =
  Name.camelCase functionName + "_" + FFI.typeName @a Proxy

ffiName2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  (a -> b -> value -> result) ->
  Text
ffiName2 functionName _ =
  Text.join "_" $
    [ Name.camelCase functionName
    , FFI.typeName @a Proxy
    , FFI.typeName @b Proxy
    ]

ffiName3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  (a -> b -> c -> value -> result) ->
  Text
ffiName3 functionName _ =
  Text.join "_" $
    [ Name.camelCase functionName
    , FFI.typeName @a Proxy
    , FFI.typeName @b Proxy
    , FFI.typeName @c Proxy
    ]

ffiName4 ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  (a -> b -> c -> d -> value -> result) ->
  Text
ffiName4 functionName _ =
  Text.join "_" $
    [ Name.camelCase functionName
    , FFI.typeName @a Proxy
    , FFI.typeName @b Proxy
    , FFI.typeName @c Proxy
    , FFI.typeName @d Proxy
    ]

invoke :: MemberFunction value -> Ptr () -> Ptr () -> IO ()
invoke function = case function of
  MemberFunction0 f ->
    \inputPtr outputPtr -> IO.do
      self <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f self)
  MemberFunction0U f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  MemberFunction0M f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  MemberFunction1 _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 self)
  MemberFunction1U _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  MemberFunction1M _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  MemberFunction2 _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 self)
  MemberFunction2U _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  MemberFunction2M _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  MemberFunction3 _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 self)
  MemberFunction3U _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  MemberFunction3M _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  MemberFunction4 _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4 self)
  MemberFunction4U _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
  MemberFunction4M _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))
