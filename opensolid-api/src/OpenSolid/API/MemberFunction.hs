module OpenSolid.API.MemberFunction
  ( MemberFunction (..)
  , ffiName
  , invoke
  , signature
  , documentation
  )
where

import Foreign (Ptr)
import OpenSolid.API.Argument qualified as Argument
import OpenSolid.API.ImplicitTolerance (ImplicitTolerance (ImplicitTolerance))
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance

data MemberFunction where
  MemberFunction0 ::
    (FFI value, FFI result) =>
    (value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionM0 ::
    (FFI value, FFI result) =>
    (Tolerance Meters => value -> result) ->
    Text ->
    MemberFunction
  MemberFunction1 ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (a -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionM1 ::
    (FFI a, FFI value, FFI result) =>
    Name ->
    (Tolerance Meters => a -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunction2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (a -> b -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionM2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunction3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionM3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunction4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (a -> b -> c -> d -> value -> result) ->
    Text ->
    MemberFunction
  MemberFunctionM4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Name ->
    Name ->
    Name ->
    Name ->
    (Tolerance Meters => a -> b -> c -> d -> value -> result) ->
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

invoke :: MemberFunction -> Ptr () -> Ptr () -> IO ()
invoke function = case function of
  MemberFunction0 f _ ->
    \inputPtr outputPtr -> do
      self <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f self)
  MemberFunctionM0 f _ ->
    \inputPtr outputPtr -> do
      (tolerance, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f self))
  MemberFunction1 _ f _ ->
    \inputPtr outputPtr -> do
      (arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 self)
  MemberFunctionM1 _ f _ ->
    \inputPtr outputPtr -> do
      (tolerance, arg1, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 self))
  MemberFunction2 _ _ f _ ->
    \inputPtr outputPtr -> do
      (arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 self)
  MemberFunctionM2 _ _ f _ ->
    \inputPtr outputPtr -> do
      (tolerance, arg1, arg2, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 self))
  MemberFunction3 _ _ _ f _ ->
    \inputPtr outputPtr -> do
      (arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 self)
  MemberFunctionM3 _ _ _ f _ ->
    \inputPtr outputPtr -> do
      (tolerance, arg1, arg2, arg3, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 self))
  MemberFunction4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> do
      (arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f arg1 arg2 arg3 arg4 self)
  MemberFunctionM4 _ _ _ _ f _ ->
    \inputPtr outputPtr -> do
      (tolerance, arg1, arg2, arg3, arg4, self) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (Tolerance.using tolerance (f arg1 arg2 arg3 arg4 self))

type Signature = (Maybe ImplicitTolerance, List (Name, FFI.Type, Argument.Kind), FFI.Type)

normalizeSignature ::
  (Maybe ImplicitTolerance, List (Name, FFI.Type, Argument.Kind), FFI.Type) ->
  (Maybe ImplicitTolerance, List (Name, FFI.Type), List (Name, FFI.Type), FFI.Type)
normalizeSignature (maybeImplicitTolerance, arguments, returnType) =
  if not (List.isOrdered (\(_, _, kind1) (_, _, kind2) -> kind1 <= kind2) arguments)
    then InternalError.throw "Named arguments should always come after positional arguments"
    else do
      let args desiredKind = [(name, typ) | (name, typ, kind) <- arguments, kind == desiredKind]
      (maybeImplicitTolerance, args Argument.Positional, args Argument.Named, returnType)

signature ::
  MemberFunction ->
  (Maybe ImplicitTolerance, List (Name, FFI.Type), List (Name, FFI.Type), FFI.Type)
signature memberFunction = normalizeSignature $ case memberFunction of
  MemberFunction0 f _ -> signature0 f
  MemberFunctionM0 f _ -> signatureM0 f
  MemberFunction1 arg1 f _ -> signature1 arg1 f
  MemberFunctionM1 arg1 f _ -> signatureM1 arg1 f
  MemberFunction2 arg1 arg2 f _ -> signature2 arg1 arg2 f
  MemberFunctionM2 arg1 arg2 f _ -> signatureM2 arg1 arg2 f
  MemberFunction3 arg1 arg2 arg3 f _ -> signature3 arg1 arg2 arg3 f
  MemberFunctionM3 arg1 arg2 arg3 f _ -> signatureM3 arg1 arg2 arg3 f
  MemberFunction4 arg1 arg2 arg3 arg4 f _ -> signature4 arg1 arg2 arg3 arg4 f
  MemberFunctionM4 arg1 arg2 arg3 arg4 f _ -> signatureM4 arg1 arg2 arg3 arg4 f

arg :: forall t -> FFI t => Name -> (Name, FFI.Type, Argument.Kind)
arg t name = (name, FFI.typeOf t, Argument.kind t)

signature0 ::
  forall value result.
  (FFI value, FFI result) =>
  (value -> result) ->
  Signature
signature0 _ = (Nothing, [], FFI.typeOf result)

signatureM0 ::
  forall value result.
  (FFI value, FFI result) =>
  (Tolerance Meters => value -> result) ->
  Signature
signatureM0 _ =
  (Just ImplicitTolerance, [], FFI.typeOf result)

signature1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (a -> value -> result) ->
  Signature
signature1 arg1 _ =
  (Nothing, [arg a arg1], FFI.typeOf result)

signatureM1 ::
  forall a value result.
  (FFI a, FFI value, FFI result) =>
  Name ->
  (Tolerance Meters => a -> value -> result) ->
  Signature
signatureM1 arg1 _ = (Just ImplicitTolerance, [arg a arg1], FFI.typeOf result)

signature2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  (a -> b -> value -> result) ->
  Signature
signature2 arg1 arg2 _ =
  (Nothing, [arg a arg1, arg b arg2], FFI.typeOf result)

signatureM2 ::
  forall a b value result.
  (FFI a, FFI b, FFI value, FFI result) =>
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> value -> result) ->
  Signature
signatureM2 arg1 arg2 _ =
  (Just ImplicitTolerance, [arg a arg1, arg b arg2], FFI.typeOf result)

signature3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (a -> b -> c -> value -> result) ->
  Signature
signature3 arg1 arg2 arg3 _ =
  (Nothing, [arg a arg1, arg b arg2, arg c arg3], FFI.typeOf result)

signatureM3 ::
  forall a b c value result.
  (FFI a, FFI b, FFI c, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> value -> result) ->
  Signature
signatureM3 arg1 arg2 arg3 _ =
  (Just ImplicitTolerance, [arg a arg1, arg b arg2, arg c arg3], FFI.typeOf result)

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
  (Nothing, [arg a arg1, arg b arg2, arg c arg3, arg d arg4], FFI.typeOf result)

signatureM4 ::
  forall a b c d value result.
  (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
  Name ->
  Name ->
  Name ->
  Name ->
  (Tolerance Meters => a -> b -> c -> d -> value -> result) ->
  Signature
signatureM4 arg1 arg2 arg3 arg4 _ =
  (Just ImplicitTolerance, [arg a arg1, arg b arg2, arg c arg3, arg d arg4], FFI.typeOf result)

documentation :: MemberFunction -> Text
documentation memberFunction = case memberFunction of
  MemberFunction0 _ docs -> docs
  MemberFunctionM0 _ docs -> docs
  MemberFunction1 _ _ docs -> docs
  MemberFunctionM1 _ _ docs -> docs
  MemberFunction2 _ _ _ docs -> docs
  MemberFunctionM2 _ _ _ docs -> docs
  MemberFunction3 _ _ _ _ docs -> docs
  MemberFunctionM3 _ _ _ _ docs -> docs
  MemberFunction4 _ _ _ _ _ docs -> docs
  MemberFunctionM4 _ _ _ _ _ docs -> docs
