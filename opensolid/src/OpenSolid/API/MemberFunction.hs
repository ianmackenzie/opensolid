module OpenSolid.API.MemberFunction
  ( MemberFunction (..)
  , ffiName
  , invoke
  , signature
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import IO qualified
import List qualified
import OpenSolid
import OpenSolid.API.Constraint (Constraint (..))
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Pair qualified
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
ffiName functionName memberFunction = do
  let (_, arguments, _, _) = signature memberFunction
  let argumentTypes = List.map Pair.second arguments
  Text.join "_" (Name.camelCase functionName : List.map FFI.typeName argumentTypes)

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

type Signature = (Maybe Constraint, List (Name, FFI.Type), FFI.Type, FFI.Type)

signature :: MemberFunction value -> (Maybe Constraint, List (Name, FFI.Type), FFI.Type, FFI.Type)
signature staticFunction = case staticFunction of
  MemberFunction0 f -> signature0 f
  MemberFunction0U f -> signature0U f
  MemberFunction0M f -> signature0M f
  MemberFunction1 arg1 f -> signature1 arg1 f
  MemberFunction1U arg1 f -> signature1U arg1 f
  MemberFunction1M arg1 f -> signature1M arg1 f
  MemberFunction2 arg1 arg2 f -> signature2 arg1 arg2 f
  MemberFunction2U arg1 arg2 f -> signature2U arg1 arg2 f
  MemberFunction2M arg1 arg2 f -> signature2M arg1 arg2 f
  MemberFunction3 arg1 arg2 arg3 f -> signature3 arg1 arg2 arg3 f
  MemberFunction3U arg1 arg2 arg3 f -> signature3U arg1 arg2 arg3 f
  MemberFunction3M arg1 arg2 arg3 f -> signature3M arg1 arg2 arg3 f
  MemberFunction4 arg1 arg2 arg3 arg4 f -> signature4 arg1 arg2 arg3 arg4 f
  MemberFunction4U arg1 arg2 arg3 arg4 f -> signature4U arg1 arg2 arg3 arg4 f
  MemberFunction4M arg1 arg2 arg3 arg4 f -> signature4M arg1 arg2 arg3 arg4 f

signature0 ::
  forall value result.
  (FFI value, FFI result) =>
  (value -> result) ->
  Signature
signature0 _ = (Nothing, [], FFI.typeOf @value Proxy, FFI.typeOf @result Proxy)

signature0U ::
  forall value result.
  (FFI value, FFI result) =>
  (Tolerance Unitless => value -> result) ->
  Signature
signature0U _ = (Just ToleranceUnitless, [], FFI.typeOf @value Proxy, FFI.typeOf @result Proxy)

signature0M ::
  forall value result.
  (FFI value, FFI result) =>
  (Tolerance Meters => value -> result) ->
  Signature
signature0M _ = (Just ToleranceMeters, [], FFI.typeOf @value Proxy, FFI.typeOf @result Proxy)

signature1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (a -> value -> result) ->
  Signature
signature1 arg1 _ =
  ( Nothing
  , [(arg1, FFI.typeOf @a Proxy)]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature1U ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (Tolerance Unitless => a -> value -> result) ->
  Signature
signature1U arg1 _ =
  ( Just ToleranceUnitless
  , [(arg1, FFI.typeOf @a Proxy)]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature1M ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (Tolerance Meters => a -> value -> result) ->
  Signature
signature1M arg1 _ =
  ( Just ToleranceMeters
  , [(arg1, FFI.typeOf @a Proxy)]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  (a -> b -> value -> result) ->
  Signature
signature2 arg1 arg2 _ =
  ( Nothing
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    ]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature2U ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> value -> result) ->
  Signature
signature2U arg1 arg2 _ =
  ( Just ToleranceUnitless
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    ]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature2M ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> value -> result) ->
  Signature
signature2M arg1 arg2 _ =
  ( Just ToleranceMeters
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    ]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> value -> result) ->
  Signature
signature3 arg1 arg2 arg3 _ =
  ( Nothing
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    ]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature3U ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> value -> result) ->
  Signature
signature3U arg1 arg2 arg3 _ =
  ( Just ToleranceUnitless
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    ]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature3M ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> value -> result) ->
  Signature
signature3M arg1 arg2 arg3 _ =
  ( Just ToleranceMeters
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    ]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature4 ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> value -> result) ->
  Signature
signature4 arg1 arg2 arg3 arg4 _ =
  ( Nothing
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    , (arg4, FFI.typeOf @d Proxy)
    ]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature4U ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Unitless => a -> b -> c -> d -> value -> result) ->
  Signature
signature4U arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceUnitless
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    , (arg4, FFI.typeOf @d Proxy)
    ]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )

signature4M ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d -> value -> result) ->
  Signature
signature4M arg1 arg2 arg3 arg4 _ =
  ( Just ToleranceMeters
  ,
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    , (arg4, FFI.typeOf @d Proxy)
    ]
  , FFI.typeOf @value Proxy
  , FFI.typeOf @result Proxy
  )
