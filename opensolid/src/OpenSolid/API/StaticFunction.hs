module OpenSolid.API.StaticFunction
  ( StaticFunction (..)
  , ffiName
  , invoke
  , arguments
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import IO qualified
import OpenSolid
import OpenSolid.API.Argument (Argument (Argument))
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Text qualified
import Tolerance qualified
import Units (Meters)

data StaticFunction where
  StaticFunction0 :: FFI a => a -> StaticFunction
  StaticFunction0U :: FFI a => (Tolerance Unitless => a) -> StaticFunction
  StaticFunction0M :: FFI a => (Tolerance Meters => a) -> StaticFunction
  StaticFunction1 :: (FFI a, FFI b) => Name -> (a -> b) -> StaticFunction
  StaticFunction1U :: (FFI a, FFI b) => Name -> (Tolerance Unitless => a -> b) -> StaticFunction
  StaticFunction1M :: (FFI a, FFI b) => Name -> (Tolerance Meters => a -> b) -> StaticFunction
  StaticFunction2 ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (a -> b -> c) ->
    StaticFunction
  StaticFunction2U ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c) ->
    StaticFunction
  StaticFunction2M ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c) ->
    StaticFunction
  StaticFunction3 ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d) ->
    StaticFunction
  StaticFunction3U ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c -> d) ->
    StaticFunction
  StaticFunction3M ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d) ->
    StaticFunction
  StaticFunction4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> e) ->
    StaticFunction
  StaticFunction4U ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c -> d -> e) ->
    StaticFunction
  StaticFunction4M ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> e) ->
    StaticFunction

-- TODO include e.g. 'ToleranceMeters' in FFI name, for explicitness?
ffiName :: Name -> StaticFunction -> Text
ffiName functionName staticFunction = case staticFunction of
  StaticFunction0 _ -> Name.camelCase functionName
  StaticFunction0U _ -> Name.camelCase functionName
  StaticFunction0M _ -> Name.camelCase functionName
  StaticFunction1 _ f -> ffiName1 functionName f
  StaticFunction1U _ f -> ffiName1 functionName (Tolerance.exactly f)
  StaticFunction1M _ f -> ffiName1 functionName (Tolerance.exactly f)
  StaticFunction2 _ _ f -> ffiName2 functionName f
  StaticFunction2U _ _ f -> ffiName2 functionName (Tolerance.exactly f)
  StaticFunction2M _ _ f -> ffiName2 functionName (Tolerance.exactly f)
  StaticFunction3 _ _ _ f -> ffiName3 functionName f
  StaticFunction3U _ _ _ f -> ffiName3 functionName (Tolerance.exactly f)
  StaticFunction3M _ _ _ f -> ffiName3 functionName (Tolerance.exactly f)
  StaticFunction4 _ _ _ _ f -> ffiName4 functionName f
  StaticFunction4U _ _ _ _ f -> ffiName4 functionName (Tolerance.exactly f)
  StaticFunction4M _ _ _ _ f -> ffiName4 functionName (Tolerance.exactly f)

ffiName1 :: forall a value. (FFI a, FFI value) => Name -> (a -> value) -> Text
ffiName1 functionName _ =
  Name.camelCase functionName + "_" + FFI.typeName @a Proxy

ffiName2 :: forall a b value. (FFI a, FFI b, FFI value) => Name -> (a -> b -> value) -> Text
ffiName2 functionName _ =
  Text.join "_" $
    [ Name.camelCase functionName
    , FFI.typeName @a Proxy
    , FFI.typeName @b Proxy
    ]

ffiName3 ::
  forall a b c value.
  (FFI a, FFI b, FFI c, FFI value) =>
  Name ->
  (a -> b -> c -> value) ->
  Text
ffiName3 functionName _ =
  Text.join "_" $
    [ Name.camelCase functionName
    , FFI.typeName @a Proxy
    , FFI.typeName @b Proxy
    , FFI.typeName @c Proxy
    ]

ffiName4 ::
  forall a b c d value.
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Name ->
  (a -> b -> c -> d -> value) ->
  Text
ffiName4 functionName _ =
  Text.join "_" $
    [ Name.camelCase functionName
    , FFI.typeName @a Proxy
    , FFI.typeName @b Proxy
    , FFI.typeName @c Proxy
    , FFI.typeName @d Proxy
    ]

invoke :: StaticFunction -> Ptr () -> Ptr () -> IO ()
invoke function = case function of
  StaticFunction0 v ->
    \_ outputPtr -> FFI.store outputPtr 0 v
  StaticFunction0U v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  StaticFunction0M v ->
    \inputPtr outputPtr -> IO.do
      tolerance <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance v)
  StaticFunction1 _ f ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  StaticFunction1U _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  StaticFunction1M _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  StaticFunction2 _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  StaticFunction2U _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  StaticFunction2M _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  StaticFunction3 _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  StaticFunction3U _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  StaticFunction3M _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  StaticFunction4 _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)
  StaticFunction4U _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  StaticFunction4M _ _ _ _ f ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))

-- TODO update to return new-style Constraint value
arguments :: StaticFunction -> List Argument
arguments staticFunction = case staticFunction of
  StaticFunction0 v -> arguments0 v
  StaticFunction0U v -> arguments0U v
  StaticFunction0M v -> arguments0M v
  StaticFunction1 arg1 f -> arguments1 arg1 f
  StaticFunction1U arg1 f -> arguments1U arg1 f
  StaticFunction1M arg1 f -> arguments1M arg1 f
  StaticFunction2 arg1 arg2 f -> arguments2 arg1 arg2 f
  StaticFunction2U arg1 arg2 f -> arguments2U arg1 arg2 f
  StaticFunction2M arg1 arg2 f -> arguments2M arg1 arg2 f
  StaticFunction3 arg1 arg2 arg3 f -> arguments3 arg1 arg2 arg3 f
  StaticFunction3U arg1 arg2 arg3 f -> arguments3U arg1 arg2 arg3 f
  StaticFunction3M arg1 arg2 arg3 f -> arguments3M arg1 arg2 arg3 f
  StaticFunction4 arg1 arg2 arg3 arg4 f -> arguments4 arg1 arg2 arg3 arg4 f
  StaticFunction4U arg1 arg2 arg3 arg4 f -> arguments4U arg1 arg2 arg3 arg4 f
  StaticFunction4M arg1 arg2 arg3 arg4 f -> arguments4M arg1 arg2 arg3 arg4 f

arguments0 :: forall a. FFI a => a -> List Argument
arguments0 _ = []

arguments0U :: forall a. FFI a => (Tolerance Unitless => a) -> List Argument
arguments0U _ = []

arguments0M :: forall a. FFI a => (Tolerance Meters => a) -> List Argument
arguments0M _ = []

arguments1 ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (a -> b) ->
  List Argument
arguments1 arg1 _ = [Argument @a Proxy arg1]

arguments1U ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (Tolerance Unitless => a -> b) ->
  List Argument
arguments1U arg1 _ = [Argument @a Proxy arg1]

arguments1M ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (Tolerance Meters => a -> b) ->
  List Argument
arguments1M arg1 _ = [Argument @a Proxy arg1]

arguments2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (a -> b -> c) ->
  List Argument
arguments2 arg1 arg2 _ = [Argument @a Proxy arg1, Argument @b Proxy arg2]

arguments2U ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c) ->
  List Argument
arguments2U arg1 arg2 _ = [Argument @a Proxy arg1, Argument @b Proxy arg2]

arguments2M ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c) ->
  List Argument
arguments2M arg1 arg2 _ = [Argument @a Proxy arg1, Argument @b Proxy arg2]

arguments3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d) ->
  List Argument
arguments3 arg1 arg2 arg3 _ =
  [ Argument @a Proxy arg1
  , Argument @b Proxy arg2
  , Argument @c Proxy arg3
  ]

arguments3U ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> d) ->
  List Argument
arguments3U arg1 arg2 arg3 _ =
  [ Argument @a Proxy arg1
  , Argument @b Proxy arg2
  , Argument @c Proxy arg3
  ]

arguments3M ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d) ->
  List Argument
arguments3M arg1 arg2 arg3 _ =
  [ Argument @a Proxy arg1
  , Argument @b Proxy arg2
  , Argument @c Proxy arg3
  ]

arguments4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> e) ->
  List Argument
arguments4 arg1 arg2 arg3 arg4 _ =
  [ Argument @a Proxy arg1
  , Argument @b Proxy arg2
  , Argument @c Proxy arg3
  , Argument @d Proxy arg4
  ]

arguments4U ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> d -> e) ->
  List Argument
arguments4U arg1 arg2 arg3 arg4 _ =
  [ Argument @a Proxy arg1
  , Argument @b Proxy arg2
  , Argument @c Proxy arg3
  , Argument @d Proxy arg4
  ]

arguments4M ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d -> e) ->
  List Argument
arguments4M arg1 arg2 arg3 arg4 _ =
  [ Argument @a Proxy arg1
  , Argument @b Proxy arg2
  , Argument @c Proxy arg3
  , Argument @d Proxy arg4
  ]
