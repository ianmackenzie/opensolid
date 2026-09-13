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
    (FFI a, FFI b) =>
    Name ->
    (a -> IO b) ->
    Text ->
    StaticFunction
  StaticFunctionM1 ::
    (FFI a, FFI b) =>
    Name ->
    (Tolerance Meters => a -> IO b) ->
    Text ->
    StaticFunction
  StaticFunction2 ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (a -> b -> IO c) ->
    Text ->
    StaticFunction
  StaticFunctionM2 ::
    (FFI a, FFI b, FFI c) =>
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> IO c) ->
    Text ->
    StaticFunction
  StaticFunction3 ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> IO d) ->
    Text ->
    StaticFunction
  StaticFunctionM3 ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> IO d) ->
    Text ->
    StaticFunction
  StaticFunction4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> IO e) ->
    Text ->
    StaticFunction
  StaticFunctionM4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> IO e) ->
    Text ->
    StaticFunction
  StaticFunction5 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> e -> IO f) ->
    Text ->
    StaticFunction
  StaticFunctionM5 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> e -> IO f) ->
    Text ->
    StaticFunction
  StaticFunction6 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g) =>
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> e -> f -> IO g) ->
    Text ->
    StaticFunction
  StaticFunctionM6 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g) =>
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> e -> f -> IO g) ->
    Text ->
    StaticFunction
  StaticFunction10 ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g, FFI h, FFI i, FFI j, FFI k) =>
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
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> IO k) ->
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

type Signature = (Maybe ImplicitTolerance, List (Name, FFI.Type, Argument.Kind), FFI.Type)

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
  StaticFunction1 arg1 f _ -> signature1 arg1 f
  StaticFunctionM1 arg1 f _ -> signatureM1 arg1 f
  StaticFunction2 arg1 arg2 f _ -> signature2 arg1 arg2 f
  StaticFunctionM2 arg1 arg2 f _ -> signatureM2 arg1 arg2 f
  StaticFunction3 arg1 arg2 arg3 f _ -> signature3 arg1 arg2 arg3 f
  StaticFunctionM3 arg1 arg2 arg3 f _ -> signatureM3 arg1 arg2 arg3 f
  StaticFunction4 arg1 arg2 arg3 arg4 f _ -> signature4 arg1 arg2 arg3 arg4 f
  StaticFunctionM4 arg1 arg2 arg3 arg4 f _ -> signatureM4 arg1 arg2 arg3 arg4 f
  StaticFunction5 arg1 arg2 arg3 arg4 arg5 f _ -> signature5 arg1 arg2 arg3 arg4 arg5 f
  StaticFunctionM5 arg1 arg2 arg3 arg4 arg5 f _ -> signatureM5 arg1 arg2 arg3 arg4 arg5 f
  StaticFunction6 arg1 arg2 arg3 arg4 arg5 arg6 f _ -> signature6 arg1 arg2 arg3 arg4 arg5 arg6 f
  StaticFunctionM6 arg1 arg2 arg3 arg4 arg5 arg6 f _ -> signatureM6 arg1 arg2 arg3 arg4 arg5 arg6 f
  StaticFunction10 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 f _ -> signature10 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 f

arg :: forall t -> FFI t => Name -> (Name, FFI.Type, Argument.Kind)
arg t name = (name, FFI.typeOf t, Argument.kind t)

signature1 ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (a -> IO b) ->
  Signature
signature1 arg1 _ =
  (Nothing, [arg a arg1], FFI.typeOf b)

signatureM1 ::
  forall a b.
  (FFI a, FFI b) =>
  Name ->
  (Tolerance Meters => a -> IO b) ->
  Signature
signatureM1 arg1 _ =
  (Just ImplicitTolerance, [arg a arg1], FFI.typeOf b)

signature2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (a -> b -> IO c) ->
  Signature
signature2 arg1 arg2 _ =
  (Nothing, [arg a arg1, arg b arg2], FFI.typeOf c)

signatureM2 ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> IO c) ->
  Signature
signatureM2 arg1 arg2 _ =
  (Just ImplicitTolerance, [arg a arg1, arg b arg2], FFI.typeOf c)

signature3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> IO d) ->
  Signature
signature3 arg1 arg2 arg3 _ =
  (Nothing, [arg a arg1, arg b arg2, arg c arg3], FFI.typeOf d)

signatureM3 ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> IO d) ->
  Signature
signatureM3 arg1 arg2 arg3 _ =
  (Just ImplicitTolerance, [arg a arg1, arg b arg2, arg c arg3], FFI.typeOf d)

signature4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> IO e) ->
  Signature
signature4 arg1 arg2 arg3 arg4 _ =
  (Nothing, [arg a arg1, arg b arg2, arg c arg3, arg d arg4], FFI.typeOf e)

signatureM4 ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d -> IO e) ->
  Signature
signatureM4 arg1 arg2 arg3 arg4 _ =
  (Just ImplicitTolerance, [arg a arg1, arg b arg2, arg c arg3, arg d arg4], FFI.typeOf e)

signature5 ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Name ->
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> e -> IO f) ->
  Signature
signature5 arg1 arg2 arg3 arg4 arg5 _ =
  (Nothing, [arg a arg1, arg b arg2, arg c arg3, arg d arg4, arg e arg5], FFI.typeOf f)

signatureM5 ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Name ->
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d -> e -> IO f) ->
  Signature
signatureM5 arg1 arg2 arg3 arg4 arg5 _ =
  (Just ImplicitTolerance, [arg a arg1, arg b arg2, arg c arg3, arg d arg4, arg e arg5], FFI.typeOf f)

signature6 ::
  forall a b c d e f g.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g) =>
  Name ->
  Name ->
  Name ->
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> d -> e -> f -> IO g) ->
  Signature
signature6 arg1 arg2 arg3 arg4 arg5 arg6 _ =
  (Nothing, [arg a arg1, arg b arg2, arg c arg3, arg d arg4, arg e arg5, arg f arg6], FFI.typeOf g)

signatureM6 ::
  forall a b c d e f g.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g) =>
  Name ->
  Name ->
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d -> e -> f -> IO g) ->
  Signature
signatureM6 arg1 arg2 arg3 arg4 arg5 arg6 _ =
  (Just ImplicitTolerance, [arg a arg1, arg b arg2, arg c arg3, arg d arg4, arg e arg5, arg f arg6], FFI.typeOf g)

signature10 ::
  forall a b c d e f g h i j k.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g, FFI h, FFI i, FFI j, FFI k) =>
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
  (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> IO k) ->
  Signature
signature10 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 _ =
  (Nothing, [arg a arg1, arg b arg2, arg c arg3, arg d arg4, arg e arg5, arg f arg6, arg g arg7, arg h arg8, arg i arg9, arg j arg10], FFI.typeOf k)

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
