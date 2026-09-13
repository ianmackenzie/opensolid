module OpenSolid.API.StaticFunction
  ( StaticFunction (..)
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

data StaticFunction where
  StaticFunction1 ::
    (FFI a, FFI result) =>
    Name ->
    (a -> IO result) ->
    Text ->
    StaticFunction
  StaticFunctionM1 ::
    (FFI a, FFI result) =>
    Name ->
    (Tolerance Meters => a -> IO result) ->
    Text ->
    StaticFunction
  StaticFunction2 ::
    (FFI a, FFI b, FFI result) =>
    Name ->
    Name ->
    (a -> b -> IO result) ->
    Text ->
    StaticFunction
  StaticFunctionM2 ::
    (FFI a, FFI b, FFI result) =>
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> IO result) ->
    Text ->
    StaticFunction
  StaticFunction3 ::
    (FFI a, FFI b, FFI c, FFI result) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> IO result) ->
    Text ->
    StaticFunction
  StaticFunctionM3 ::
    (FFI a, FFI b, FFI c, FFI result) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> IO result) ->
    Text ->
    StaticFunction
  StaticFunction4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> IO result) ->
    Text ->
    StaticFunction
  StaticFunctionM4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> IO result) ->
    Text ->
    StaticFunction
  StaticFunction5 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> e -> IO result) ->
    Text ->
    StaticFunction
  StaticFunctionM5 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> e -> IO result) ->
    Text ->
    StaticFunction
  StaticFunction6 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> e -> f -> IO result) ->
    Text ->
    StaticFunction
  StaticFunctionM6 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> e -> f -> IO result) ->
    Text ->
    StaticFunction
  StaticFunction10 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g, FFI h, FFI i, FFI j, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> IO result) ->
    Text ->
    StaticFunction

ffiName :: FFI.ClassName -> Name -> StaticFunction -> Text
ffiName className functionName staticFunction = do
  let (_, positionalArguments, namedArguments, _) = signature staticFunction
  let arguments = positionalArguments <> namedArguments
  let argumentTypes = List.map Pair.second arguments
  Text.join "_" $
    "opensolid"
      : FFI.concatenatedName className
      : FFI.camelCase functionName
      : List.map FFI.typeName argumentTypes

invoke :: StaticFunction -> FFI.Function
invoke function = case function of
  StaticFunction1 _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      arg1 <- FFI.load inputPtr 0
      FFI.invoke (f arg1) outputPtr errorPtr
  StaticFunctionM1 _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, arg1) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f arg1)) outputPtr errorPtr
  StaticFunction2 _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2) outputPtr errorPtr
  StaticFunctionM2 _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, arg1, arg2) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f arg1 arg2)) outputPtr errorPtr
  StaticFunction3 _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2 arg3) outputPtr errorPtr
  StaticFunctionM3 _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, arg1, arg2, arg3) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f arg1 arg2 arg3)) outputPtr errorPtr
  StaticFunction4 _ _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2 arg3 arg4) outputPtr errorPtr
  StaticFunctionM4 _ _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, arg1, arg2, arg3, arg4) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f arg1 arg2 arg3 arg4)) outputPtr errorPtr
  StaticFunction5 _ _ _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2, arg3, arg4, arg5) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2 arg3 arg4 arg5) outputPtr errorPtr
  StaticFunctionM5 _ _ _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, arg1, arg2, arg3, arg4, arg5) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 arg5)) outputPtr errorPtr
  StaticFunction6 _ _ _ _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2, arg3, arg4, arg5, arg6) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2 arg3 arg4 arg5 arg6) outputPtr errorPtr
  StaticFunctionM6 _ _ _ _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (tolerance, arg1, arg2, arg3, arg4, arg5, arg6) <- FFI.load inputPtr 0
      FFI.invoke (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 arg5 arg6)) outputPtr errorPtr
  StaticFunction10 _ _ _ _ _ _ _ _ _ _ f _ ->
    \inputPtr outputPtr errorPtr -> do
      (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) <- FFI.load inputPtr 0
      FFI.invoke (f arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) outputPtr errorPtr

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
  StaticFunction ->
  (Maybe ImplicitTolerance, List (Name, FFI.Type), List (Name, FFI.Type), FFI.Type)
signature staticFunction = normalizeSignature $ case staticFunction of
  StaticFunction1 @a @result arg1 _ _ ->
    ( Nothing
    , [arg a arg1]
    , FFI.typeOf result
    )
  StaticFunctionM1 @a @result arg1 _ _ ->
    ( Just ImplicitTolerance
    , [arg a arg1]
    , FFI.typeOf result
    )
  StaticFunction2 @a @b @result arg1 arg2 _ _ ->
    ( Nothing
    , [arg a arg1, arg b arg2]
    , FFI.typeOf result
    )
  StaticFunctionM2 @a @b @result arg1 arg2 _ _ ->
    ( Just ImplicitTolerance
    , [arg a arg1, arg b arg2]
    , FFI.typeOf result
    )
  StaticFunction3 @a @b @c @result arg1 arg2 arg3 _ _ ->
    ( Nothing
    , [arg a arg1, arg b arg2, arg c arg3]
    , FFI.typeOf result
    )
  StaticFunctionM3 @a @b @c @result arg1 arg2 arg3 _ _ ->
    ( Just ImplicitTolerance
    , [arg a arg1, arg b arg2, arg c arg3]
    , FFI.typeOf result
    )
  StaticFunction4 @a @b @c @d @result arg1 arg2 arg3 arg4 _ _ ->
    ( Nothing
    , [arg a arg1, arg b arg2, arg c arg3, arg d arg4]
    , FFI.typeOf result
    )
  StaticFunctionM4 @a @b @c @d @result arg1 arg2 arg3 arg4 _ _ ->
    ( Just ImplicitTolerance
    , [arg a arg1, arg b arg2, arg c arg3, arg d arg4]
    , FFI.typeOf result
    )
  StaticFunction5 @a @b @c @d @e @result arg1 arg2 arg3 arg4 arg5 _ _ ->
    ( Nothing
    , [arg a arg1, arg b arg2, arg c arg3, arg d arg4, arg e arg5]
    , FFI.typeOf result
    )
  StaticFunctionM5 @a @b @c @d @e @result arg1 arg2 arg3 arg4 arg5 _ _ ->
    ( Just ImplicitTolerance
    , [arg a arg1, arg b arg2, arg c arg3, arg d arg4, arg e arg5]
    , FFI.typeOf result
    )
  StaticFunction6 @a @b @c @d @e @f @result arg1 arg2 arg3 arg4 arg5 arg6 _ _ ->
    ( Nothing
    , [arg a arg1, arg b arg2, arg c arg3, arg d arg4, arg e arg5, arg f arg6]
    , FFI.typeOf result
    )
  StaticFunctionM6 @a @b @c @d @e @f @result arg1 arg2 arg3 arg4 arg5 arg6 _ _ ->
    ( Just ImplicitTolerance
    , [arg a arg1, arg b arg2, arg c arg3, arg d arg4, arg e arg5, arg f arg6]
    , FFI.typeOf result
    )
  StaticFunction10 @a @b @c @d @e @f @g @h @i @j @result arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 _ _ ->
    ( Nothing
    , [arg a arg1, arg b arg2, arg c arg3, arg d arg4, arg e arg5, arg f arg6, arg g arg7, arg h arg8, arg i arg9, arg j arg10]
    , FFI.typeOf result
    )

arg :: forall t -> FFI t => Name -> (Name, FFI.Type, Argument.Kind)
arg t name = (name, FFI.typeOf t, Argument.kind t)

documentation :: StaticFunction -> Text
documentation memberFunction = case memberFunction of
  StaticFunction1 _ _ docs -> docs
  StaticFunctionM1 _ _ docs -> docs
  StaticFunction2 _ _ _ docs -> docs
  StaticFunctionM2 _ _ _ docs -> docs
  StaticFunction3 _ _ _ _ docs -> docs
  StaticFunctionM3 _ _ _ _ docs -> docs
  StaticFunction4 _ _ _ _ _ docs -> docs
  StaticFunctionM4 _ _ _ _ _ docs -> docs
  StaticFunction5 _ _ _ _ _ _ docs -> docs
  StaticFunctionM5 _ _ _ _ _ _ docs -> docs
  StaticFunction6 _ _ _ _ _ _ _ docs -> docs
  StaticFunctionM6 _ _ _ _ _ _ _ docs -> docs
  StaticFunction10 _ _ _ _ _ _ _ _ _ _ _ docs -> docs
