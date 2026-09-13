module OpenSolid.API.MemberFunction
  ( MemberFunction (..)
  , ffiName
  , invoke
  , signature
  , documentation
  )
where

import OpenSolid.API.Argument qualified as Argument
import OpenSolid.API.ImplicitTolerance (ImplicitTolerance (ImplicitTolerance))
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance

data MemberFunction where
  MemberFunction0 ::
    (FFI value, FFI result) =>
    (value -> IO result) ->
    Text ->
    MemberFunction
  MemberFunctionT0 ::
    (FFI value, FFI result) =>
    (Tolerance Meters => value -> IO result) ->
    Text ->
    MemberFunction
  MemberFunction1 ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (a -> value -> IO result) ->
    Text ->
    MemberFunction
  MemberFunctionT1 ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (Tolerance Meters => a -> value -> IO result) ->
    Text ->
    MemberFunction
  MemberFunction2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (a -> b -> value -> IO result) ->
    Text ->
    MemberFunction
  MemberFunctionT2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> value -> IO result) ->
    Text ->
    MemberFunction
  MemberFunction3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> value -> IO result) ->
    Text ->
    MemberFunction
  MemberFunctionT3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> value -> IO result) ->
    Text ->
    MemberFunction
  MemberFunction4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> value -> IO result) ->
    Text ->
    MemberFunction
  MemberFunctionT4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> value -> IO result) ->
    Text ->
    MemberFunction

ffiName :: FFI.ClassName -> Name -> MemberFunction -> Text
ffiName className functionName memberFunction = do
  let (_, positionalArguments, namedArguments, _) = signature memberFunction
  let arguments = positionalArguments <> namedArguments
  let argumentTypes = List.map Pair.second arguments
  Text.join "_" $
    "opensolid"
      : FFI.concatenatedName className
      : FFI.camelCase functionName
      : List.map FFI.typeName argumentTypes

invoke :: MemberFunction -> FFI.Function
invoke function = case function of
  MemberFunction0 f _ ->
    \inputPtr outputPtr errorPtr -> do
      self <- FFI.load inputPtr 0
      FFI.invoke (f self) outputPtr errorPtr
  MemberFunctionT0 f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f self)) outputPtr errorPtr
  MemberFunction1 _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, self) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 self) outputPtr errorPtr
  MemberFunctionT1 _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f arg1 self)) outputPtr errorPtr
  MemberFunction2 _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2 self) outputPtr errorPtr
  MemberFunctionT2 _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f arg1 arg2 self)) outputPtr errorPtr
  MemberFunction3 _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2 arg3 self) outputPtr errorPtr
  MemberFunctionT3 _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f arg1 arg2 arg3 self)) outputPtr errorPtr
  MemberFunction4 _ _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2 arg3 arg4 self) outputPtr errorPtr
  MemberFunctionT4 _ _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self)) outputPtr errorPtr

normalizeSignature ::
  (Maybe ImplicitTolerance, List (Name, FFI.Type, Argument.Kind), FFI.Type) ->
  (Maybe ImplicitTolerance, List (Name, FFI.Type), List (Name, FFI.Type), FFI.Type)
normalizeSignature (maybeImplicitTolerance, arguments, returnType) =
  if not (List.isOrdered (\(_, _, kind1) (_, _, kind2) -> kind1 <= kind2) arguments)
    then error "Named arguments should always come after positional arguments"
    else do
      let args desiredKind = [(name, typ) | (name, typ, kind) <- arguments, kind == desiredKind]
      (maybeImplicitTolerance, args Argument.Positional, args Argument.Named, returnType)

signature ::
  MemberFunction ->
  (Maybe ImplicitTolerance, List (Name, FFI.Type), List (Name, FFI.Type), FFI.Type)
signature memberFunction = normalizeSignature $ case memberFunction of
  MemberFunction0 @_ @result _ _ ->
    ( Nothing
    , []
    , FFI.typeOf result
    )
  MemberFunctionT0 @_ @result _ _ ->
    ( Just ImplicitTolerance
    , []
    , FFI.typeOf result
    )
  MemberFunction1 @a @_ @result arg1 _ _ ->
    ( Nothing
    , [arg a arg1]
    , FFI.typeOf result
    )
  MemberFunctionT1 @a @_ @result arg1 _ _ ->
    ( Just ImplicitTolerance
    , [arg a arg1]
    , FFI.typeOf result
    )
  MemberFunction2 @a @b @_ @result arg1 arg2 _ _ ->
    ( Nothing
    , [arg a arg1, arg b arg2]
    , FFI.typeOf result
    )
  MemberFunctionT2 @a @b @_ @result arg1 arg2 _ _ ->
    ( Just ImplicitTolerance
    , [arg a arg1, arg b arg2]
    , FFI.typeOf result
    )
  MemberFunction3 @a @b @c @_ @result arg1 arg2 arg3 _ _ ->
    ( Nothing
    , [arg a arg1, arg b arg2, arg c arg3]
    , FFI.typeOf result
    )
  MemberFunctionT3 @a @b @c @_ @result arg1 arg2 arg3 _ _ ->
    ( Just ImplicitTolerance
    , [arg a arg1, arg b arg2, arg c arg3]
    , FFI.typeOf result
    )
  MemberFunction4 @a @b @c @d @_ @result arg1 arg2 arg3 arg4 _ _ ->
    ( Nothing
    , [arg a arg1, arg b arg2, arg c arg3, arg d arg4]
    , FFI.typeOf result
    )
  MemberFunctionT4 @a @b @c @d @_ @result arg1 arg2 arg3 arg4 _ _ ->
    ( Just ImplicitTolerance
    , [arg a arg1, arg b arg2, arg c arg3, arg d arg4]
    , FFI.typeOf result
    )

arg :: forall t -> FFI t => Name -> (Name, FFI.Type, Argument.Kind)
arg t name = (name, FFI.typeOf t, Argument.kind t)

documentation :: MemberFunction -> Text
documentation memberFunction = case memberFunction of
  MemberFunction0 _ docs -> docs
  MemberFunctionT0 _ docs -> docs
  MemberFunction1 _ _ docs -> docs
  MemberFunctionT1 _ _ docs -> docs
  MemberFunction2 _ _ _ docs -> docs
  MemberFunctionT2 _ _ _ docs -> docs
  MemberFunction3 _ _ _ _ docs -> docs
  MemberFunctionT3 _ _ _ _ docs -> docs
  MemberFunction4 _ _ _ _ _ docs -> docs
  MemberFunctionT4 _ _ _ _ _ docs -> docs
