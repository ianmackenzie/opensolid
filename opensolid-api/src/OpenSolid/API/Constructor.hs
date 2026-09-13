module OpenSolid.API.Constructor
  ( Constructor (..)
  , ffiName
  , invoke
  , signature
  , documentation
  )
where

import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Constructor where
  Constructor1 ::
    (FFI a, FFI value) =>
    Name ->
    (a -> IO value) ->
    Text ->
    Constructor
  Constructor2 ::
    (FFI a, FFI b, FFI value) =>
    Name ->
    Name ->
    (a -> b -> IO value) ->
    Text ->
    Constructor
  Constructor3 ::
    (FFI a, FFI b, FFI c, FFI value) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> IO value) ->
    Text ->
    Constructor
  Constructor4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> IO value) ->
    Text ->
    Constructor

ffiName :: FFI.ClassName -> Constructor -> Text
ffiName className constructor = do
  let arguments = signature constructor
  let argumentTypes = List.map Pair.second arguments
  Text.join "_" $
    "opensolid"
      : FFI.concatenatedName className
      : "constructor"
      : List.map FFI.typeName argumentTypes

invoke :: Constructor -> FFI.Function
invoke function = case function of
  Constructor1 _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      arg1 <- FFI.load inputPtr 0
      FFI.invoke (f arg1) outputPtr errorPtr
  Constructor2 _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2) outputPtr errorPtr
  Constructor3 _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2 arg3) outputPtr errorPtr
  Constructor4 _ _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2 arg3 arg4) outputPtr errorPtr

signature :: Constructor -> List (Name, FFI.Type)
signature constructor = case constructor of
  Constructor1 @a arg1 _ _ ->
    [ (arg1, FFI.typeOf a)
    ]
  Constructor2 @a @b arg1 arg2 _ _ ->
    [ (arg1, FFI.typeOf a)
    , (arg2, FFI.typeOf b)
    ]
  Constructor3 @a @b @c arg1 arg2 arg3 _ _ ->
    [ (arg1, FFI.typeOf a)
    , (arg2, FFI.typeOf b)
    , (arg3, FFI.typeOf c)
    ]
  Constructor4 @a @b @c @d arg1 arg2 arg3 arg4 _ _ ->
    [ (arg1, FFI.typeOf a)
    , (arg2, FFI.typeOf b)
    , (arg3, FFI.typeOf c)
    , (arg4, FFI.typeOf d)
    ]

documentation :: Constructor -> Text
documentation constructor = case constructor of
  Constructor1 _ _ docs -> docs
  Constructor2 _ _ _ docs -> docs
  Constructor3 _ _ _ _ docs -> docs
  Constructor4 _ _ _ _ _ docs -> docs
