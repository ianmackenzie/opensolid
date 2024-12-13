module API.StaticFunction
  ( StaticFunction (..)
  , ffiName
  , invoke
  , signature
  , documentation
  )
where

import API.Constraint (Constraint (..))
import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import IO qualified
import List qualified
import OpenSolid
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Text qualified
import Tolerance qualified
import Units (Meters)

data StaticFunction where
  StaticFunction1 ::
    (FFI a, FFI b) =>
    Name ->
    (a -> b) ->
    Text ->
    StaticFunction
  StaticFunction1U ::
    (FFI a, FFI b) =>
    Name ->
    (Tolerance Unitless => a -> b) ->
    Text ->
    StaticFunction
  StaticFunction1R ::
    (FFI a, FFI b) =>
    Name ->
    (Tolerance Radians => a -> b) ->
    Text ->
    StaticFunction
  StaticFunction1M ::
    (FFI a, FFI b) =>
    Name ->
    (Tolerance Meters => a -> b) ->
    Text ->
    StaticFunction
  StaticFunction2 ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (a -> b -> c) ->
    Text ->
    StaticFunction
  StaticFunction2U ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c) ->
    Text ->
    StaticFunction
  StaticFunction2R ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (Tolerance Radians => a -> b -> c) ->
    Text ->
    StaticFunction
  StaticFunction2M ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c) ->
    Text ->
    StaticFunction
  StaticFunction3 ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d) ->
    Text ->
    StaticFunction
  StaticFunction3U ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c -> d) ->
    Text ->
    StaticFunction
  StaticFunction3R ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Radians => a -> b -> c -> d) ->
    Text ->
    StaticFunction
  StaticFunction3M ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d) ->
    Text ->
    StaticFunction
  StaticFunction4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> e) ->
    Text ->
    StaticFunction
  StaticFunction4U ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Unitless => a -> b -> c -> d -> e) ->
    Text ->
    StaticFunction
  StaticFunction4R ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Radians => a -> b -> c -> d -> e) ->
    Text ->
    StaticFunction
  StaticFunction4M ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> e) ->
    Text ->
    StaticFunction

ffiName :: FFI.Id a -> Name -> StaticFunction -> Text
ffiName classId functionName memberFunction = do
  let (_, arguments, _) = signature memberFunction
  let argumentTypes = List.map Pair.second arguments
  Text.join "_" $
    "opensolid"
      : FFI.className classId
      : FFI.camelCase functionName
      : List.map FFI.typeName argumentTypes

invoke :: StaticFunction -> Ptr () -> Ptr () -> IO ()
invoke function = case function of
  StaticFunction1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  StaticFunction1U _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  StaticFunction1R _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  StaticFunction1M _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1))
  StaticFunction2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  StaticFunction2U _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  StaticFunction2R _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  StaticFunction2M _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2))
  StaticFunction3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  StaticFunction3U _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  StaticFunction3R _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  StaticFunction3M _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3))
  StaticFunction4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)
  StaticFunction4U _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  StaticFunction4R _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))
  StaticFunction4M _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4))

type Signature = (Maybe Constraint, List (Name, FFI.Type), FFI.Type)

signature :: StaticFunction -> (Maybe Constraint, List (Name, FFI.Type), FFI.Type)
signature staticFunction = case staticFunction of
  StaticFunction1 arg1 f _ -> signature1 arg1 f
  StaticFunction1U arg1 f _ -> signature1U arg1 f
  StaticFunction1R arg1 f _ -> signature1R arg1 f
  StaticFunction1M arg1 f _ -> signature1M arg1 f
  StaticFunction2 arg1 arg2 f _ -> signature2 arg1 arg2 f
  StaticFunction2U arg1 arg2 f _ -> signature2U arg1 arg2 f
  StaticFunction2R arg1 arg2 f _ -> signature2R arg1 arg2 f
  StaticFunction2M arg1 arg2 f _ -> signature2M arg1 arg2 f
  StaticFunction3 arg1 arg2 arg3 f _ -> signature3 arg1 arg2 arg3 f
  StaticFunction3U arg1 arg2 arg3 f _ -> signature3U arg1 arg2 arg3 f
  StaticFunction3R arg1 arg2 arg3 f _ -> signature3R arg1 arg2 arg3 f
  StaticFunction3M arg1 arg2 arg3 f _ -> signature3M arg1 arg2 arg3 f
  StaticFunction4 arg1 arg2 arg3 arg4 f _ -> signature4 arg1 arg2 arg3 arg4 f
  StaticFunction4U arg1 arg2 arg3 arg4 f _ -> signature4U arg1 arg2 arg3 arg4 f
  StaticFunction4R arg1 arg2 arg3 arg4 f _ -> signature4R arg1 arg2 arg3 arg4 f
  StaticFunction4M arg1 arg2 arg3 arg4 f _ -> signature4M arg1 arg2 arg3 arg4 f

signature1 ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (a -> b) ->
  Signature
signature1 arg1 _ = (Nothing, [(arg1, FFI.typeOf @a Proxy)], FFI.typeOf @b Proxy)

signature1U ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (Tolerance Unitless => a -> b) ->
  Signature
signature1U arg1 _ = (Just ToleranceUnitless, [(arg1, FFI.typeOf @a Proxy)], FFI.typeOf @b Proxy)

signature1R ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (Tolerance Radians => a -> b) ->
  Signature
signature1R arg1 _ = (Just ToleranceRadians, [(arg1, FFI.typeOf @a Proxy)], FFI.typeOf @b Proxy)

signature1M ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (Tolerance Meters => a -> b) ->
  Signature
signature1M arg1 _ = (Just ToleranceMeters, [(arg1, FFI.typeOf @a Proxy)], FFI.typeOf @b Proxy)

signature2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (a -> b -> c) ->
  Signature
signature2 arg1 arg2 _ =
  (Nothing, [(arg1, FFI.typeOf @a Proxy), (arg2, FFI.typeOf @b Proxy)], FFI.typeOf @c Proxy)

signature2U ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c) ->
  Signature
signature2U arg1 arg2 _ =
  (Just ToleranceUnitless, [(arg1, FFI.typeOf @a Proxy), (arg2, FFI.typeOf @b Proxy)], FFI.typeOf @c Proxy)

signature2R ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (Tolerance Radians => a -> b -> c) ->
  Signature
signature2R arg1 arg2 _ =
  (Just ToleranceRadians, [(arg1, FFI.typeOf @a Proxy), (arg2, FFI.typeOf @b Proxy)], FFI.typeOf @c Proxy)

signature2M ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c) ->
  Signature
signature2M arg1 arg2 _ =
  (Just ToleranceMeters, [(arg1, FFI.typeOf @a Proxy), (arg2, FFI.typeOf @b Proxy)], FFI.typeOf @c Proxy)

signature3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d) ->
  Signature
signature3 arg1 arg2 arg3 _ =
  ( Nothing
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    ]
  , FFI.typeOf @d Proxy
  )

signature3U ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> d) ->
  Signature
signature3U arg1 arg2 arg3 _ =
  ( Just ToleranceUnitless
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    ]
  , FFI.typeOf @d Proxy
  )

signature3R ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Radians => a -> b -> c -> d) ->
  Signature
signature3R arg1 arg2 arg3 _ =
  ( Just ToleranceRadians
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    ]
  , FFI.typeOf @d Proxy
  )

signature3M ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d) ->
  Signature
signature3M arg1 arg2 arg3 _ =
  ( Just ToleranceMeters
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    ]
  , FFI.typeOf @d Proxy
  )

signature4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> e) ->
  Signature
signature4 arg1 arg2 arg3 arg4 _ =
  ( Nothing
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    , (arg4, FFI.typeOf @d Proxy)
    ]
  , FFI.typeOf @e Proxy
  )

signature4U ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> d -> e) ->
  Signature
signature4U arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceUnitless
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    , (arg4, FFI.typeOf @d Proxy)
    ]
  , FFI.typeOf @e Proxy
  )

signature4R ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Radians => a -> b -> c -> d -> e) ->
  Signature
signature4R arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceRadians
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    , (arg4, FFI.typeOf @d Proxy)
    ]
  , FFI.typeOf @e Proxy
  )

signature4M ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d -> e) ->
  Signature
signature4M arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceMeters
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    , (arg4, FFI.typeOf @d Proxy)
    ]
  , FFI.typeOf @e Proxy
  )

documentation :: StaticFunction -> Text
documentation memberFunction = case memberFunction of
  StaticFunction1 _ _ docs -> docs
  StaticFunction1U _ _ docs -> docs
  StaticFunction1R _ _ docs -> docs
  StaticFunction1M _ _ docs -> docs
  StaticFunction2 _ _ _ docs -> docs
  StaticFunction2U _ _ _ docs -> docs
  StaticFunction2R _ _ _ docs -> docs
  StaticFunction2M _ _ _ docs -> docs
  StaticFunction3 _ _ _ _ docs -> docs
  StaticFunction3U _ _ _ _ docs -> docs
  StaticFunction3R _ _ _ _ docs -> docs
  StaticFunction3M _ _ _ _ docs -> docs
  StaticFunction4 _ _ _ _ _ docs -> docs
  StaticFunction4U _ _ _ _ _ docs -> docs
  StaticFunction4R _ _ _ _ _ docs -> docs
  StaticFunction4M _ _ _ _ _ docs -> docs
