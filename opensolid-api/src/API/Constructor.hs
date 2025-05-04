module API.Constructor
  ( Constructor (..)
  , ffiName
  , invoke
  , signature
  , documentation
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Constructor value where
  Constructor1 ::
    (FFI a, FFI value) =>
    Name ->
    (a -> value) ->
    Text ->
    Constructor value
  Constructor2 ::
    (FFI a, FFI b, FFI value) =>
    Name ->
    Name ->
    (a -> b -> value) ->
    Text ->
    Constructor value
  Constructor3 ::
    (FFI a, FFI b, FFI c, FFI value) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> value) ->
    Text ->
    Constructor value
  Constructor4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> value) ->
    Text ->
    Constructor value

ffiName :: FFI.Class -> Constructor value -> Text
ffiName ffiClass constructor = do
  let (arguments, _) = signature constructor
  let argumentTypes = List.map Pair.second arguments
  Text.join "_" $
    "opensolid"
      : FFI.concatenatedName ffiClass
      : "constructor"
      : List.map FFI.typeName argumentTypes

invoke :: Constructor value -> Ptr () -> Ptr () -> IO ()
invoke function = case function of
  Constructor1 _ f _ ->
    \inputPtr outputPtr -> IO.do
      arg1 <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1)
  Constructor2 _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2)
  Constructor3 _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3)
  Constructor4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> IO.do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4)

type Signature = (List (Name, FFI.Type), FFI.Type)

signature :: Constructor value -> (List (Name, FFI.Type), FFI.Type)
signature constructor = case constructor of
  Constructor1 arg1 f _ -> signature1 arg1 f
  Constructor2 arg1 arg2 f _ -> signature2 arg1 arg2 f
  Constructor3 arg1 arg2 arg3 f _ -> signature3 arg1 arg2 arg3 f
  Constructor4 arg1 arg2 arg3 arg4 f _ -> signature4 arg1 arg2 arg3 arg4 f

signature1 ::
  forall a value.
  (FFI a, FFI value) =>
  Name ->
  (a -> value) ->
  Signature
signature1 arg1 _ = ([(arg1, FFI.typeOf @a Proxy)], FFI.typeOf @value Proxy)

signature2 ::
  forall a b value.
  (FFI a, FFI b, FFI value) =>
  Name ->
  Name ->
  (a -> b -> value) ->
  Signature
signature2 arg1 arg2 _ =
  ([(arg1, FFI.typeOf @a Proxy), (arg2, FFI.typeOf @b Proxy)], FFI.typeOf @value Proxy)

signature3 ::
  forall a b c value.
  (FFI a, FFI b, FFI c, FFI value) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> value) ->
  Signature
signature3 arg1 arg2 arg3 _ =
  ( [(arg1, FFI.typeOf @a Proxy), (arg2, FFI.typeOf @b Proxy), (arg3, FFI.typeOf @c Proxy)]
  , FFI.typeOf @value Proxy
  )

signature4 ::
  forall a b c d value.
  (FFI a, FFI b, FFI c, FFI d, FFI value) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> value) ->
  Signature
signature4 arg1 arg2 arg3 arg4 _ =
  (
    [ (arg1, FFI.typeOf @a Proxy)
    , (arg2, FFI.typeOf @b Proxy)
    , (arg3, FFI.typeOf @c Proxy)
    , (arg4, FFI.typeOf @d Proxy)
    ]
  , FFI.typeOf @value Proxy
  )

documentation :: Constructor value -> Text
documentation constructor = case constructor of
  Constructor1 _ _ docs -> docs
  Constructor2 _ _ _ docs -> docs
  Constructor3 _ _ _ _ docs -> docs
  Constructor4 _ _ _ _ _ docs -> docs
